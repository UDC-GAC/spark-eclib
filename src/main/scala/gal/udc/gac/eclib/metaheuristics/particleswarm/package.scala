/*
 * eclib - Spark Evolutionary Computing Library
 * Copyright © 2022 Xoán C. Pardo (xoan.pardo@udc.gal)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package gal.udc.gac.eclib.metaheuristics

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.language.{implicitConversions, postfixOps}
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.util.LongAccumulator
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.experiment.SparkExperimentContext
import gal.udc.gac.eclib.modules.{PropertiesStore, PropertyName}
import gal.udc.gac.eclib.modules.individualimprovers.IndividualImprover.IndividualImprovementFunction
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.population._
import gal.udc.gac.eclib.population.IndividualWithID._
import gal.udc.gac.eclib.metaheuristics.particleswarm.Particle.ParticleOrdering
import gal.udc.gac.eclib.metaheuristics.particleswarm.neighborhood.{NeighborhoodContribution, NeighborhoodOps, ParticleContributionFunction}
import gal.udc.gac.eclib.metaheuristics.particleswarm.steps.VelocityInitialization.{IndexedVelocityInitializationFunction, VelocityInitializationFunction}
import gal.udc.gac.eclib.util.duration

/** PSO metaheuristic implemented reusing the definitions and default implementations
 * already provided in [[BaseEvolutionaryMetaheuristic]]
 */
package object particleswarm {
  /** =Types and implementations reused from [[BaseEvolutionaryMetaheuristic]]= */
  /** The base algorithm structure that also provides default implementations
   *  of the population generation and termination condition steps
   */
  //type BaseAlgorithm = BaseEvolutionaryMetaheuristic.Implementations.Default.Algorithm
  /** the definition of the steps to be implemented */
  //type EvolutionImplementation = BaseEvolutionaryMetaheuristic.EvolutionImplementation

  /** ------------------------------------------------------------------------------------------ */

  // enumeration types of algorithm parameters stored in the properties store
  case object InertiaWeightW extends PropertyName
  case object CognitiveCoefficientC1 extends PropertyName
  case object SocialCoefficientC2 extends PropertyName
  case object ConstrictionFactorX extends PropertyName
  case object SwarmTopology extends PropertyName

  // types of other stored properties
  case object SwarmSize extends PropertyName        // actual swarm size (in some cases it can be different from configured size)
  case object SwarmInitialID extends PropertyName     // initial value of the particle IDs (default = 0)
  case object GlobalBest extends PropertyName       // (global) best individual of all iterations
  case object VelocityLimitVmax extends PropertyName
  case object LastGlobalBest extends PropertyName   // global best of the last iteration
  case object GenerationsWithoutImprovement extends PropertyName

  // island-related properties
  type IslandSizes = Vector[Int]
  case object IslandSizes extends PropertyName      // island sizes
  type IslandProperties = Vector[PropertiesStore]
  case object IslandProperties extends PropertyName  // island properties


  /** ------------------------------------------------------------------------------------------ */

  type Velocity = Vector[Double]
  object Velocity {
    def Zero(sz: Int): Velocity = Vector.fill(sz)(0.0)
    def Unit(sz: Int): Velocity = Vector.fill(sz)(1.0)
    def MaxValue(sz: Int): Velocity = Vector.fill(sz)(Double.MaxValue)
    def MinValue(sz: Int): Velocity = Vector.fill(sz)(Double.MinValue)
  }

  // function type to bound a velocity to a velocity limit
  type VelocityBoundFunction = (Velocity, Bound) => Velocity

  // bound a velocity to a velocity limit using a given bound function
  implicit def toBoundedVelocity(v: Velocity) = new {
    def bound(f: VelocityBoundFunction)(vmax: Bound): Velocity = f(v, vmax) // the new bounded velocity
  }

  // function type to bound the movement (position and velocity) of a particle to search space limits
  type Movement = (Position, Velocity)
  type MovementBoundFunction = (Movement, Bound, Bound) => Movement

  object Particle {
    def apply(id: ParticleID, p: Position, f: Double = Double.MaxValue)(implicit vInit: VelocityInitializationFunction): Particle =
      Particle(id, p, f, vInit(), Individual(p, f))
    def apply(id: ParticleID, i: Individual)(implicit vInit: VelocityInitializationFunction): Particle = Particle(id, i.element, i.fitness)
    def apply(i: IndividualWithID)(implicit vInit: VelocityInitializationFunction): Particle = Particle(i.id, i.element, i.fitness)

    // implicit ordering of particles is by fitness
    implicit object ParticleOrdering extends Ordering[Particle] {
      def compare(a: Particle, b: Particle): Int = a.fitness compare b.fitness
    }

    // bound a particle to search space limits using given bound function
    implicit def toBoundedParticle(p: Particle) = new {
      def bound(f: MovementBoundFunction)(params: SearchSpaceParameters): Particle =
        if (params.lowerLimits.isEmpty) p
        else {
          val (pos, vel) = f((p.element, p.velocity), params.lowerLimits.get, params.upperLimits.get) // bounded pos and velocity
          Particle(p.id, pos, BAD_POINT, vel, p.best) // the new bounded particle (yet to be evaluated)
        }
    }
  }

  // function type to move a particle that is a member of a swarm
  type ParticleMovementFunction = Particle => Particle
  // factory of particle movement functions
  type ParticleMovementFunctionFactory = Swarm => ParticleMovementFunction
  type ParticleMovementFunctionFactories = Vector[ParticleMovementFunctionFactory]

  type ParticleID = IndividualID

  case class Particle(
                       override val id: ParticleID,
                       override val element: Position,
                       override val fitness: Double,
                       velocity: Velocity,
                       best: Individual
                     ) extends IndividualWithID(id, element, fitness) {

    override def toString: String = s"Particle[${super.toString}, Velocity(${velocity.mkString(", ")}), best=$best]"

    override def evaluate(implicit f: FitnessFunction): Particle = {
      val fitness = f(element)
      Particle(id, element, fitness, velocity, if (fitness < best.fitness) Individual(element, fitness) else best)
    }

    // TODO: check how to improve particles (is velocity improved as well?).
    //  This method is only an adaptation of the individual improvement method
    override def improve(implicit f: IndividualImprovementFunction): (Particle, Evaluations) = {
      val (ind, eval) = f(this)
      (Particle(id, ind.element, ind.fitness, velocity, if (ind.fitness < best.fitness) ind else best), eval)
    }

    /**
     * Move a particle using a given movement function
     *
     * @param f the movement function
     * @return the new particle after having moved the original
     */
    def move(f: ParticleMovementFunction): Particle = f(this)
  }

  type Particles = Vector[Particle]
  object Particles {
    def apply(): Particles = Vector[Particle]()
  }

  /** ------------------------------------------------------------------------------------------ */

  /** Swarms are a type of population */
  type Swarm = Population with SwarmOps with NeighborhoodOps

  trait SwarmOps {
    def historyBest(): Individual
    def move()(implicit f: ParticleMovementFunctionFactory): Swarm
    def filter(cond: Particle => Boolean): Swarm  // returns a new swarm with only the particles that meet a given condition
  }

  object Swarm {
    /** Create an empty Swarm */
    def apply(): GroupedSwarm = GroupedSwarm()
    /** create a swarm from a collection of particles */
    def apply(p: Particles): GroupedSwarm = GroupedSwarm(p)
    /** create a swarm from a population of individuals */
    def apply(p: Population)(implicit vInit: IndexedVelocityInitializationFunction): Swarm = p.asSwarm()
  }

  implicit class PopulationAsSwarm(p: Population) {
    /**
     * Creates a swarm from a population
     *
     * @param vInit the velocity initialization function (a "decorator" for a collection of functions indexed by position)
     * @return the new swarm
     * @note the zipWithIndex method is different in Scala and Spark.
     *       Whereas Scala's indexes are of type Int, Spark's are of type Long.
     *       Also zipWithIndex in Spark requires to trigger a job.
     */
    def asSwarm()(implicit vInit: IndexedVelocityInitializationFunction): Swarm = p match {
      case gp: GroupedPopulation =>
        // use individual's index in the collection of individuals as particle's identifier and
        // 0 as index for the velocity initialization function (there is only 1 function for grouped populations)
        GroupedSwarm(gp.zipWithIndex.map((v: (Individual, Int)) =>
          Particle(v._1.withId(v._2))(vInit(0))))
      case dp: SparkDistributedPopulation =>
        // use individual's index in the RDD converted to Int as particle's identifier and
        // the partition ID as index for the velocity initialization function (each partition might
        // have its own initialization function). Note that the resulting RDD is cached and it is
        // assumed that the population size is less or equal than Int.Maxvalue
        DistributedSwarm(dp.zipWithIndex.mapPartitionsWithIndex((pid, individuals) =>
          individuals.map{ case (individual, id) =>
            Particle(individual.withId(id.toInt))(vInit(pid)) }).cache)
    }
  }

  implicit def toSwarm(a: Array[Particle]): GroupedSwarm = GroupedSwarm(a.toVector)
  implicit def toSwarm(i: Iterator[Particle]): GroupedSwarm = GroupedSwarm(i.toVector)

  implicit def toParticles(s: GroupedSwarm): Particles = s.individuals.asInstanceOf[Particles]
  implicit def toSwarm(p: Particles): GroupedSwarm = GroupedSwarm(p)

  /** a grouped swarm of particles */
  class GroupedSwarm(p: Particles)
    extends GroupedPopulation(p)
      with SwarmOps
      with NeighborhoodOps {

    private def particles: Particles = individuals.asInstanceOf[Particles]

    private lazy val _historyBest: Individual = particles.map(_.best).min
    override def historyBest(): Individual = _historyBest

    override def evaluate(implicit f: FitnessFunction): GroupedSwarm = particles.map(_ evaluate)

    override def move()(implicit f: ParticleMovementFunctionFactory): GroupedSwarm = {
      val pmf: ParticleMovementFunction = f(this) // create the particle movement function using the current swarm as argument
      particles.map(_.move(pmf))  // move the particles
    }

    override def filter(cond: Particle => Boolean): GroupedSwarm = particles.filter(cond)

    // function to collect the contribution of each particle to the sociometry
    override def contribution[T](pf: ParticleContributionFunction[T])(
        implicit ct: ClassTag[T]): NeighborhoodContribution[T] =
      particles.map(particle => particle.id -> pf(particle)) toMap

    override def toString = s"GroupedSwarm[${particles.mkString(", ")}]"
  }

  object GroupedSwarm {
    /** Create an empty GroupedSwarm */
    def apply(): GroupedSwarm = new GroupedSwarm(Particles())
    /** create a swarm from a collection of particles */
    def apply(p: Particles): GroupedSwarm = new GroupedSwarm(p)
    /**
     * Adds a bound method through an implicit to bound a swarm to search space limits
     * using a given bound function
     */
    implicit def toBoundedSwarm(s: GroupedSwarm) = new {
      def bound(f: MovementBoundFunction)(params: SearchSpaceParameters): GroupedSwarm =
        if (params.lowerLimits.isEmpty) s
        else s.map(p => p.bound(f)(params))
    }
  }


  /**
   * A swarm of particles that is distributed
   */

  abstract class DistributedSwarm
    extends DistributedPopulation
      with SwarmOps
      with NeighborhoodOps
      with SwarmIslandOps

  object DistributedSwarm {
    def apply(rdd: RDD[Particle]): SparkDistributedSwarm = new SparkDistributedSwarm(rdd)
  }

  /** Swarm islands */
  type SwarmIsland = GroupedSwarm
  trait SwarmIslandOps {
    /**
     * Evolves swarm islands (partitions)
     *
     * @param from  the globally accumulated evaluations and absolute elapsed time.
     *              These values are needed to check for the termination condition
     *              after each local iteration.
     * @param steps the factory of evolution steps to evolve the islands state (a "decorator" for a collection of factories indexed by position)
     * @param tc the termination condition to check for at the end of each evolution step
     * @param accEvals an accumulator to store the number of evaluations performed during the
     *                 evolution of the islands (the total number is accesible only to the Spark driver)
     * @param f the implicit fitness function
     * @return the new distributed swarm resulting from the evolution
     */

    // TODO: remove dependencies from Spark accumulators
    // TODO: add a general approach to gather and update global state

    def evolveIslands(from: (Evaluations, Duration))(
      steps: IndexedEvolutionStepFunctionFactory, tc: TerminationConditionFunction[State])(
      accEvals: LongAccumulator)/*(log: CollectionAccumulator[(Generations, Double, Evaluations, Duration)])*/(
      implicit f: FitnessFunction): DistributedSwarm
  }

  /** Spark implementation */
  class SparkDistributedSwarm(override val rdd: RDD[Particle])
    extends DistributedSwarm
      with CachedPopulationRDD[Particle] {

    override final val ord: Ordering[Particle] = ParticleOrdering // override implicit ordering

    private lazy val _historyBest: Individual = rdd.map(_.best).min
    override def historyBest(): Individual = _historyBest

    import SparkDistributedSwarm.SparkFunctions._

    override def evaluate(implicit f: FitnessFunction): SparkDistributedSwarm =
      rdd.map(p => evaluateParticle(p))

    override def improve(implicit f: IndividualImprovementFunction): (SparkDistributedSwarm, Evaluations) = {
      val r = rdd.map(p => improveParticle(p))
      (r.keys, r.values.sum.toLong) // return improved individuals and evaluations as two separate RDDs
    }

    override def move()(implicit f: ParticleMovementFunctionFactory): SparkDistributedSwarm = {
      val pmf: ParticleMovementFunction = f(this) // create the particle movement function using the current swarm as argument
      rdd.map(p => moveParticle(p)(pmf)) // move the particles
    }

    override def filter(cond: Particle => Boolean): SparkDistributedSwarm = rdd.filter(cond)

    override def group(): GroupedSwarm = rdd.collect

    // function to collect the contribution of each particle to the sociometry
    override def contribution[T](pf: ParticleContributionFunction[T])(
        implicit ct: ClassTag[T]): NeighborhoodContribution[T] =
      rdd.map(particle => particle.id -> pf(particle)).collect.toMap

    // evolveislands implementation
    override def evolveIslands(from: (Evaluations, Duration))(
      steps: IndexedEvolutionStepFunctionFactory, tc: TerminationConditionFunction[State])(
      accEvals: LongAccumulator)/*(log: CollectionAccumulator[(Generations, Double, Evaluations, Duration)])*/(
      implicit f: FitnessFunction): SparkDistributedSwarm = {
      val (evals, time) = from
      rdd.mapPartitionsWithIndex { (pid, island) =>
        val (i: SwarmIsland, (g, e), t) = evolveIsland(island, (Generations.Zero, evals), time)(steps(pid), tc) // new island state
        accEvals.add(e - (evals + i.size)) // accumulates evaluations (the evaluations of 1 iteration are subtracted here
        // because they are accounted for afterwards in the global evolution)
        // log.add(g, i.best.fitness, e, t)
        i.toIterator
      }
    }
  }

  object SparkDistributedSwarm {

    // Functions to be distributed to the Spark executors
    object SparkFunctions {
      /** Functions that work with particles */
      def evaluateParticle(p: Particle)(implicit f: FitnessFunction): Particle = p.evaluate
      def improveParticle(p: Particle)(implicit f: IndividualImprovementFunction): (Particle, Evaluations) = p.improve
      def moveParticle(p: Particle)(f: ParticleMovementFunction): Particle = p.move(f)

      /** Functions that work with partitions (islands) */

      /**
       * Evolves the island state
       *
       * @param state the current state. It is assumed to accumulate the number of local iterations
       *              in the generations field, but the global accumulated evaluations and absolute
       *              elapsed time in the others. The global values are needed to check for the
       *              termination condition after each local iteration.
       * @param step the evolution step to evolve the island state (i.e. sequential PSO evolution)
       * @param tc the termination condition to check for at the end of each evolution step
       * @param f the implicit fitness function
       * @return the new island state resulting from the evolution
       */

        // TODO: reimplement using the general evolveUntil function. Check for coherence between local and global state.
        /*
        import gal.udc.gac.eclib.population._
        import gal.udc.gac.eclib.metaheuristics.BaseEvolutionaryMetaheuristic.IterationState.AccumulableIterationState
        implicit val converged = tc
        val ns = state evolveUntil step
         */

      @tailrec
      def evolveIsland(state: State)(step: EvolutionStepFunction, tc: TerminationConditionFunction[State])(
          implicit f: FitnessFunction): State = {

        val (condition, t) = duration { tc(state) } // check for the termination condition
        if (condition) state // duration is not added here because it is already accounted for in the global evolution step
        else {
          val (island: SwarmIsland, (iterations, evals), timeElapsed) = state
          val (ni: SwarmIsland, (_, ev), tt) = step(island, (iterations, evals), timeElapsed + t) // evolve the state
          evolveIsland(ni, (iterations + Generations.Unit, evals + ev + island.size), tt)(step, tc) // the island size is added here
                                                                                                    // because ev accumulates only extra evaluations (e.g. a local search)
        }
      }
    } // SparkFunctions

  }// SparkDistributedSwarm

  implicit def toRDD(s: SparkDistributedSwarm): RDD[Particle] = s.rdd
  implicit def toSwarm(rdd: RDD[Particle]): SparkDistributedSwarm = DistributedSwarm(rdd)

  implicit class GroupedtoDistributedSwarm(s: GroupedSwarm) {
    // TODO: support other execution contexts besides Spark
    def distribute(sc: SparkExperimentContext): SparkDistributedSwarm =
      DistributedSwarm(sc.parallelize(s))
    def distribute(sc: SparkExperimentContext, numSlices: Int): SparkDistributedSwarm =
      DistributedSwarm(sc.parallelize(s, numSlices))
  }

}