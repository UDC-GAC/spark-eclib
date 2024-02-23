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
package gal.udc.gac.eclib.metaheuristics.particleswarm

import scala.concurrent.duration.Duration
import com.typesafe.scalalogging.LazyLogging
import org.apache.spark.util.{CollectionAccumulator, LongAccumulator}
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.experiment._
import gal.udc.gac.eclib.modules._
import gal.udc.gac.eclib.modules.elementevaluators.ElementEvaluator
import gal.udc.gac.eclib.modules.terminationconditions._
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.population._
import gal.udc.gac.eclib.metaheuristics._
import gal.udc.gac.eclib.metaheuristics.particleswarm.steps.SwarmMovementStep.IndexedParticleMovementFunctionFactory
import gal.udc.gac.eclib.metaheuristics.particleswarm.steps._
import gal.udc.gac.eclib.util.duration

/**
 * PSO metaheuristic implemented reusing the definitions and default implementations
 * already provided in [[BaseEvolutionaryMetaheuristic]]
 **/
object ParticleSwarmOptimization extends LazyLogging {

  import Implicits._

  /**
   * Evolution trait to be mixed in the PSO algorithm
   *
   * The custom implementation (sequential, Spark, ...) of the evolution step is
   * implemented in the companion object
   *
   *  @note - the onEvolutionIterationStart hook is overridden in the algorithm
   *        but it could have been overridden here as well
   **/
  trait Evolution
    extends EvolutionImplementation {
    self: AlgorithmStep
      with PropertiesStorage
      with ElementEvaluator
      with SwarmMovementStep =>

    private object Factory
      extends RequiredStrategyFactory[EvolutionStepFunction] {
      val name = "ParticleSwarmOptimization.Evolution"
        /**
         * Two implicits are passed here to the constructor:
         *  - the properties store to access to the stored absolute elapsed time (it
         *    is necessary to check for the termination condition in the islands)
         *  - the particle move function factory to avoid having 1 algorithm
         *    for each evolution implementation.
         */
      val strategy: Option[EvolutionStepFunction] = Evolution.Implementation()
    }

    override def evolve: EvolutionStepFunction = Factory()
  }

  /** ==The algorithm of the ParticleSwarmOptimization metaheuristic==
   * It is defined by extending the base algorithm [[BaseEvolutionaryMetaheuristic.Implementations.Default.Algorithm]]
   * and mixing in the custom implementations of the evolution step.
   *
   * @note The experiment parameters are required to be of the [[ParticleSwarmParameters]] type.
   **/
  private[particleswarm]
  case class Algorithm(implicit override val ep: ExperimentParameters,
                       override val ec: ExperimentContext)
    extends BaseAlgorithm
      with ParameterInitializationAndUpdate
      with VelocityInitialization
      with Evolution
      with SwarmMovementStep {

    require(ep.algorithm match {
      case _: ParticleSwarmParameters => true
      case _ => false
    }, "Failed to configure the Particle Swarm Optimization metaheuristic: experiment parameters of type ParticleSwarmParameters not found")

    // PSO parameters (used in the overriding of hook methods)
    private val pso: ParticleSwarmParameters = ep.algorithm.asInstanceOf[ParticleSwarmParameters]

    // store initial values in the properties store
    private def initializeProperties(swarm: Swarm): Unit = {

      // store properties related with the island-based implementation
      def initializeIslandProperties() = (pso.implementation, swarm) match {
        case (_:PSOIslands, ds:SparkDistributedSwarm) =>
          // collect and store partition/island sizes
          val isz = ds.mapPartitions(p => Iterator(p.size)).collect.toVector
          propertiesStore(IslandSizes) = PropertyValue[IslandSizes](isz)
          // Create the island properties stores (each island have its own store)
          val iproperties: IslandProperties = Vector.fill(isz.size)(PropertiesStore())
          // store each island size and initial particle ID in its store (needed by some initialization functions)
          isz.indices.foreach(i => {
            // using the same keys for swarm and islands keeps implementations agnostic
            // about whether they are working with a swarm or an island
            iproperties(i)(SwarmSize) = PropertyValue[Int](isz(i))
            iproperties(i)(SwarmInitialID) = // the initial ID is the cumulative sum of previous island sizes
              PropertyValue[Int]({ if (i == 0) 0 else isz.take(i).sum })
          })
          // store the properties store of the islands as a property in the main store
          propertiesStore(IslandProperties) = PropertyValue[IslandProperties](iproperties)
        case _ => // do nothing, checking for wrong configurations is not done here
      } // initializeIslandProperties

      // store swarm static info in the properties store
      // this is a workaround to avoid launching a Spark action on each iteration to collect it
      // -- size (swarm may have less individuals than required in the configuration)
      propertiesStore(SwarmSize) = PropertyValue[Int](swarm.size)
      // -- best global particle
      propertiesStore(GlobalBest) = PropertyValue[Individual](swarm.best)
      // -- main initial ID
      propertiesStore(SwarmInitialID) = PropertyValue[Int](0)
      // store properties related with the island-based implementation
      //  note: this properties HAS TO BE stored before calling initializeAndStoreAlgorithmParameters because they are
      //        accessed there to initialize island topologies
      initializeIslandProperties()
      // initialize and store the algorithm parameters in the properties store
      initializeAndStoreAlgorithmParameters()
      logger.debug(s"Properties: $propertiesStore")

    } // initializeProperties

    /** Hook methods implementation ------------------------------ */

    // convert the initial population into a swarm
    @inline override def onPopulationGeneration = { s: State =>
      val (population, (generations, evals), timeElapsed) = s
      val (swarm, t) = duration {
        // call super to initialize the stalled generations counter
        super.onPopulationGeneration(s)
        // group or distribute the population as needed
        val p: Population = (ec, population) match {
          case (sc: SparkExperimentContext, gp: GroupedPopulation) => pso.implementation match {
            case _:PSOMWMoveAndEvaluate | _:PSOIslands => gp.distribute(sc)
            case _ => gp
          }
          case (sc: SparkExperimentContext, dp: DistributedPopulation) => pso.implementation match {
            case _:PSOMWMoveAndEvaluate | _:PSOIslands => dp
            case _ => dp.group
          }
          case _ => population // left the population as it is
        }
        // convert the population into a swarm (caching it if it is distributed)
        val swarm: Swarm = p.asSwarm
        initializeProperties(swarm)
        // log initial swarm
        //      logger.whenTraceEnabled {
        logger.whenDebugEnabled {
          swarm match {
            case gs:GroupedSwarm => logger.debug(s"Initial Swarm: $gs")
            case ds:SparkDistributedSwarm => logger.debug(s"Initial Swarm: ${ds.glom.collect}")
            // case _ =>
          }
        }
        swarm
      }
      logger.debug(s"TIME (onPopulationGeneration): $timeElapsed + $t = ${timeElapsed + t}")
      (swarm, (generations, evals), timeElapsed + t)
    }

    // log the initial state
    // TODO: check if part of the implementation can be moved to super and called from here
    @inline override def onEvolutionIterationStart = { s: State =>
      val (swarm, (generations, evals), timeElapsed) = s
      val (best, t) = duration {
        // store the absolute elapsed time at the beginning of the evolution (needed by the islands implementation)
        propertiesStore(AbsoluteElapsedTime) = PropertyValue[Duration](timeElapsed)
        // update the best global found until now (included here to account for the time spent in case the swarm is distributed)
        val best = swarm.best
        updateGlobalBest(propertiesStore, best)
        best
      }

      logger.trace(s"Properties store: $propertiesStore")
      /*************************************
        logger.whenTraceEnabled {
          if (propertiesStore.contains(InertiaWeightW)) logger.trace(s"W: ${propertiesStore(InertiaWeightW).as[Double]}  ")
          if (propertiesStore.contains(CognitiveCoefficientC1)) logger.trace(s"C1: ${propertiesStore(CognitiveCoefficientC1).as[Double]}  ")
          if (propertiesStore.contains(SocialCoefficientC2)) logger.trace(s"C2: ${propertiesStore(SocialCoefficientC2).as[Double]}  ")
          if (propertiesStore.contains(ConstrictionFactorX)) logger.trace(s"X: ${propertiesStore(ConstrictionFactorX).as[Double]}  ")
          //s", ${propertiesStore(SwarmTopology).as[Topology]}" +
          if (propertiesStore.contains(SwarmSize)) logger.trace(s"size: ${propertiesStore(SwarmSize).as[Int]}  ")
          if (propertiesStore.contains(GlobalBest)) logger.trace(s"gbest: ${propertiesStore(GlobalBest).as[Individual]}  ")
          if (propertiesStore.contains(VelocityLimitVmax)) logger.trace(s"VMax: ${propertiesStore(VelocityLimitVmax).as[Velocity]}  ")
          if (propertiesStore.contains(LastGlobalBest)) logger.trace(s"Last best: ${propertiesStore(LastGlobalBest).as[Double]}  ")
          if (propertiesStore.contains(GenerationsWithoutImprovement)) logger.trace(s"Counter: ${propertiesStore(GenerationsWithoutImprovement).as[Generations]}  ")
          if (propertiesStore.contains(IslandSizes)) logger.trace(s"Island sizes: ${propertiesStore(IslandSizes).as[Array[Int]]}")
          if (propertiesStore.contains(IslandProperties)) logger.trace(s"Island properties: ${propertiesStore(IslandProperties).as[Vector[PropertiesStore]]}")
        }
      *************************************/

//      logger.info(s"Iteration summary (generations, evaluations, time(s), iteration best, global best): " +
      logger.info(s"Iteration summary: " + // the time is logged without adding the time spent in the method
        s"${(generations, evals, timeElapsed, best, propertiesStore(GlobalBest).as[Individual]).mkString}")
      logger.debug(s"TIME (onEvolutionIterationStart): $timeElapsed + $t = ${timeElapsed + t}")
      (swarm, (generations, evals), t) // return only the time spent in the method (it is accumulated in the general evolution implementation)
    }

    /**
     * Remember to call super to update the stalled generations counter, in case this hook is overridden
     */
    @inline override def onEvolutionIterationEnd = { s: State =>
      val (swarm, (generations, evals), timeElapsed) = super.onEvolutionIterationEnd(s) // update the stalled generations counter
      val t = duration { updateStoredAlgorithmParameters(s) } // update algorithm parameters stored in the properties storage
      logger.debug(s"TIME (onEvolutionIterationEnd): ${s._3} + ${timeElapsed + t - s._3} = ${timeElapsed + t}")
      // the default value (Generations.Unit) to update the number of generations after the evolution is returned here
      // also the stored size is used to avoid collecting the swarm size on each iteration
      (swarm, (Generations.Unit, propertiesStore(SwarmSize).as[Int] + evals), timeElapsed + t)
    }

    // log the final state
    // TODO: check if part of the implementation can be moved to super and called from here
    @inline override def onTerminationCondition = { s: State =>
      val (swarm, (generations, evals), timeElapsed) = s
      val (best, t) = duration {
        logger.debug(s"$swarm")
        // update the stored global best individual
        val best = swarm.best // swarm best
        updateGlobalBest(propertiesStore, best)
        best
      }
      // logger.info(s"Iteration summary (generations, evaluations, time(s), iteration best, global best): " +
      logger.info(s"Iteration summary: " +
        s"${(generations, evals, timeElapsed, best, propertiesStore(GlobalBest).as[Individual]).mkString}")
      logger.info(s"Evolution summary (generations, evaluations, time(s), global best): " +
        s"${(generations, evals, timeElapsed + t, propertiesStore(GlobalBest).as[Individual]).mkString}")
      logger.info(s"Best solution: " + s"${propertiesStore(GlobalBest).as[Individual].element.mkString("[",",","]")}")
      logger.debug(s"TIME (onTerminationCondition): $timeElapsed + $t = ${timeElapsed + t}")

      // Used with Docker Desktop to have time to save the executor PODs output before they are removed when the execution finishes
//      Thread.sleep(30000)

      (swarm, (generations, evals), timeElapsed + t)
    }

  } // Algorithm


  /**
   * Evolution step custom implementations
   **/
  private[particleswarm]
  object Evolution {

    object Implementation {

      // For all the implementations, the default value (Generations.Unit) to update the number of generations
      // after the evolution is returned in the onEvolutionIterationEnd hook method. During the evolution the
      // generations field of the state stores the current number of generations.

      object Sequential {

        /** Sequential implementation of the evolution step */
        def apply()(implicit f: FitnessFunction, fmove: ParticleMovementFunctionFactory): EvolutionStepFunction = {
          case (swarm: GroupedSwarm, (generations, evals), timeElapsed) =>
            val (s, t) = duration { swarm.move.evaluate }
            // workaround: return only additional evaluations required in the iteration (e.g. in a local search)
            // the swarm evaluations (size of the swarm) are added in onEvolutionIterationEnd
            logger.debug(s"TIME (Evolution): $timeElapsed + $t = ${timeElapsed + t}")
            (s, (generations, Evaluations.Zero), timeElapsed + t)
        }

      } // Sequential

      object Spark {

        object MasterWorkerEvaluateOnly {

          /**
           * Spark-based master-worker implementation of the evolution step
           * The swarm is grouped and only the evaluation of particles is distributed to workers
           */
          def apply(sc: SparkExperimentContext)(implicit f: FitnessFunction, fmove: ParticleMovementFunctionFactory): EvolutionStepFunction = {
            case (swarm: GroupedSwarm, (generations, evals), timeElapsed) =>
              val (s, t) = duration { swarm.move.distribute(sc).evaluate.group }
              // workaround: return only additional evaluations required in the iteration (e.g. in a local search)
              // the evaluations needed to evaluate the swarm (size of the swarm) are added in onEvolutionIterationEnd
              logger.debug(s"TIME (Evolution): $timeElapsed + $t = ${timeElapsed + t}")
              (s, (generations, Evaluations.Zero), timeElapsed + t)
          }

        } // MasterWorkerEvaluateOnly

        object MasterWorkerMoveAndEvaluate {

          /**
           * Spark-based master-worker implementation of the evolution step
           * The swarm is distributed and both the movement and evaluation of particles is distributed to workers
           */
          def apply(sc: SparkExperimentContext)(implicit f: FitnessFunction, fmove: ParticleMovementFunctionFactory): EvolutionStepFunction = {
            case (swarm: SparkDistributedSwarm, (generations, evals), timeElapsed) =>
              val (s, t) = duration { swarm.move.evaluate.cache }
              // workaround: return only additional evaluations required in the iteration (e.g. in a local search)
              // the evaluations needed to evaluate the swarm (size of the swarm) are added in onEvolutionIterationEnd
              logger.debug(s"TIME (Evolution): $timeElapsed + $t = ${timeElapsed + t}")
              (s, (generations, Evaluations.Zero), timeElapsed + t)
          }

        } // MasterWorkerMoveAndEvaluate

        object Islands {

          /**
           * Spark-based island implementation of the evolution step
           * The swarm is distributed into islands (RDD partitions in the Spark implementation).
           * Each island executes a PSO on its own for a given number of iterations
           *
           * @param steps the factory of evolution steps to evolve the islands state (a "decorator" for a collection of factories indexed by position)
           * @param tc the termination condition to check for at the end of each evolution step
           * @param accEvals an accumulator to accumulate the number of evaluations performed during the
           *                 evolution of the islands. Note that the number of evaluations of 1 iteration
           *                 (i.e. the swarm size) has been subtracted to this value to avoid accounting for
           *                 it twice (see the comment in the source code about the evaluations workaround).
           * @param f the implicit fitness function
           * @param props a property store from which to get the absolute elapsed time at the beginning
           *              of the iteration. This is needed by the islands to check for the termination
           *              condition after each local iteration.
           * @return the evolution step function of the island model
           * @note it is assumed that the number of local iterations of the islands has nothing to do with the global
           *       generations of the population evolution. As a consequence, each execution of this evolution step adds only
           *       1 generation to the global population.
           */
          private def apply(steps: IndexedEvolutionStepFunctionFactory, tc: TerminationConditionFunction[State])(
              accEvals: LongAccumulator)/*(log: CollectionAccumulator[(Generations, Double, Evaluations, Duration)])*/(
              implicit f: FitnessFunction, props: PropertiesStore): EvolutionStepFunction = {
            case (swarm: SparkDistributedSwarm, (generations, evals), timeElapsed) =>
              val ((ns, ne), t) = duration {
                // reset accumulators
                val t1 = duration { accEvals.reset() /*; log.reset()*/ }
                // Evolve islands. Note that:
                //  - accEvals is used to accumulate the evaluations performed by each island
                //  - the globally accumulated evaluations and the absolute elapsed time are used in the islands
                //    to check for the termination condition at the end of each local iteration
                val ds = swarm.evolveIslands(evals, props(AbsoluteElapsedTime).as[Duration] + timeElapsed + t1)(steps, tc)(accEvals) /*(log)*/.cache
                // update the global best. Note that:
                //  - history best is used instead of best because islands evolved for several iterations
                //  - running the historyBest action on the swarm also serves to update the accumulators
                updateGlobalBest(props, ds.historyBest())
                // log accumulators. Note that all islands should have evolved for the same number of local iterations
                // unless the termination condition is fulfilled in any of them
                logger.debug(s"Acumulador (Evaluations): $accEvals")
                // logger.debug(s"Acumulador (Summary): $log")
                (ds, accEvals.value)
              }
            // workaround: because the evaluations needed to evaluate the swarm (i.e. size of the swarm) are added in onEvolutionIterationEnd
            // the number of accumulated evaluations by 1 iteration has already been subtracted in the islands. This avoids to launch an
            // extra Spark action to get the size of the new distributed swarm.
            logger.debug(s"TIME (Evolution): $timeElapsed + $t = ${timeElapsed + t}")
            (ns, (generations, ne), timeElapsed + t)
          }

          /** Factory for the termination condition of the islands */
          private object IslandTerminationCondition {
            /** Creates a termination condition
             *
             * @param nIter the number of local iterations
             * @param tc the termination condition parameters as specified in the configuration file
             * @return the termination condition
             * @note the termination condition checks for the number of local iterations, VTR, and absolute
             *       elapsed time and total accumulated evaluations since the beginning of the program execution
             */
            def apply(nIter: Int, tc: TerminationCriteria): TerminationConditionFunction[State] = {
              case (island, (iterations, evals), timeElapsed) =>
                NumberOfGenerationsCondition(nIter)(iterations) || // local iterations
                  NumberOfEvaluationsCondition(tc.maxEvaluations)(evals) ||
                  ValueToReachCondition(tc.targetValue)(island.best.fitness) ||
                  ElapsedExecutionTimeCondition(tc.maxTime)(timeElapsed)
            }
          }

          /** The public constructor of the evolution step function of the island model
           *
           * This constructor creates a termination condition from the parameters specified in the
           * configuration file, an accumulator to accumulate the number of evaluations performed
           * during the evolution of the islands and pass them in the call to the private constructor
           * in which the evolution step function is finally instantiated.
           *
           * @param sc the Spark context
           * @param nIter the number of local iterations
           * @param tc the termination condition parameters as specified in the configuration file
           * @param f the implicit fitness function
           * @param fmove the factory of the function to move the particles (a "decorator" for a collection of functions indexed by position)
           * @param props a property store from which to get the absolute elapsed time at the beginning
           *              of the iteration. This is needed by the islands to check for the termination
           *              condition after each local iteration.
           * @return the evolution step function of the island model
           */

          def apply(sc: SparkExperimentContext)(nIter: Int, tc: TerminationCriteria)(
              implicit f: FitnessFunction, fmove: IndexedParticleMovementFunctionFactory, props: PropertiesStore): EvolutionStepFunction = {

            // initialize island GlobalBest property
            @inline def initializeIslandGlobalBest(props: PropertiesStore, best: Individual): Unit = {
              //if (!props.contains(GlobalBest)) {
              props(GlobalBest) = PropertyValue[Individual](best)
              logger.debug(s"Initial island properties: $props")
            }

            // build the indexed factory of evolution steps (a "decorator" for a collection of factories indexed by position)
            val steps: IndexedEvolutionStepFunctionFactory =  i => { state: State =>
              val (swarm: SwarmIsland, (generations, evals), timeElapsed) = state
              val iprop = props(IslandProperties).as[IslandProperties](i) // island properties
              // logger.debug(s"TIME (before island evolution): $timeElapsed")
              logger.debug(s"Initial island members: $swarm")
              val (s, t) = duration {
                if (generations == Generations.Zero) initializeIslandGlobalBest(iprop, swarm.historyBest())
                // apply the sequential algorithm to evolve the island
                implicit def move: ParticleMovementFunctionFactory = fmove(i) // implicit movement function factory
                val step = Sequential()
                val (island: SwarmIsland, ge, d) = step(state)
                // update island GlobalBest
                updateGlobalBest(iprop, island.best)
                (island, ge, d) // return the new state
              }
              logger.whenDebugEnabled {
                logger.debug(s"TIME (island evolution): $timeElapsed + $t = ${timeElapsed + t}")
                logger.debug(s"Current island properties: $iprop")
                logger.debug(s"Current island members: ${s._1}")
              }
              logger.info(s"Island iteration summary: " +
                s"${(generations + 1, evals + s._2._2 + swarm.size, timeElapsed + t, s._1.best, iprop(GlobalBest).as[Individual]).mkString}")
              (s._1, (Generations.Unit, s._2._2 + swarm.size), t)
            }

            apply(steps, IslandTerminationCondition(nIter, tc))(sc.longAccumulator("evaluations"))/*(
              sc.collectionAccumulator[(Generations, Double, Evaluations, Duration)]("summary"))*/
          }

        } // Islands

      } // Spark

      def apply()(
        implicit ep: ExperimentParameters,
        ec: ExperimentContext,
        f: FitnessFunction,
        fmove: IndexedParticleMovementFunctionFactory,
        props: PropertiesStore): Option[EvolutionStepFunction] = {

        // define as an implicit the ParticleMovementFunctionFactory of implementations with
        // a single configuration (i.e. Sequential, Master-Worker)
        implicit def move: ParticleMovementFunctionFactory = fmove(0)

        ec match {
          // Sequential context
          case _: SequentialExperimentContext => ep.algorithm match {
            case pso: ParticleSwarmParameters => pso.implementation match {
              case _: PSOSequential => Some(Sequential())
              case _ => logger.warn("PSO sequential variant not defined or unknown"); None
            }
            case _ => None
          }
          // Spark context
          case sc: SparkExperimentContext => ep.algorithm match {
            case pso: ParticleSwarmParameters => pso.implementation match {
              case _: PSOMWEvaluateOnly => Some(Spark.MasterWorkerEvaluateOnly(sc))
              case _: PSOMWMoveAndEvaluate => Some(Spark.MasterWorkerMoveAndEvaluate(sc))
              case pso: PSOIslands => Some(Spark.Islands(sc)(pso.localIterations, ep.terminationCriteria))
              case _ => logger.warn("PSO parallel variant not defined or unknown"); None
            }
            case _ => None
          }

          // Unknown context
          case _ => None
        }
      }

    } // Implementation

  } // Evolution

  /** ==ParticleSwarmOptimization factory==
   *
   * @param ep experiment parameters
   * @param ec experiment context
   * @return an instance of the ParticleSwarmOptimization algorithm or None
   */
  def apply()(implicit ep: ExperimentParameters, ec: ExperimentContext): Option[Algorithm] = Some(Algorithm())

  private object Implicits {

    /**
     * Implicit classes to add the method mkString to tuples containing the main state information
     * @param s the tuple containing the main state information
     * @return a String with the main state information
     * @note I've tried overriding toString instead of defining a new mkString,
     *       but it doesn't work and I don't know exactly why (maybe a double implicit resolution?)
     **/
    implicit class IterationStateToString(s: (Generations, Evaluations, Duration, Individual, Individual)) {
      // return a string with the state information
      def mkString: String = s"${s._1}, ${s._2}, ${s._3.toSeconds}, " +
        s"[${s._4.fitness}], " +
        s"[${s._5.fitness}]"
//        s"[${s._4.fitness}]<-(${s._4.element.mkString(",")}), " +
//        s"[${s._5.fitness}]<-(${s._5.element.mkString(",")})"
    }

    implicit class EvolutionStateToString(s: (Generations, Evaluations, Duration, Individual)) {
      // return a string with the state information
//      def mkString: String = s"${s._1}, ${s._2}, ${s._3.toSeconds}, [${s._4.fitness}]<-(${s._4.element.mkString(",")})"
      def mkString: String = s"${s._1}, ${s._2}, ${s._3.toSeconds}, [${s._4.fitness}]"
    }

  } // Implicits

}
