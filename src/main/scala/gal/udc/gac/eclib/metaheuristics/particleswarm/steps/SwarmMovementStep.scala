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
package gal.udc.gac.eclib.metaheuristics.particleswarm.steps

import com.typesafe.scalalogging.LazyLogging
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.modules._
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.metaheuristics.particleswarm._
import gal.udc.gac.eclib.metaheuristics.particleswarm.neighborhood._
import gal.udc.gac.eclib.metaheuristics.particleswarm.steps.SwarmMovementStep.IndexedParticleMovementFunctionFactory
import gal.udc.gac.eclib.util.Random.reals

/**
 * This step defines an implicit particle movement function factory
 * for being used by Swarm move methods
 */
trait SwarmMovementStep {
  self: AlgorithmStep
    with PropertiesStorage =>

  private object Factory
   extends RequiredStrategyFactory[IndexedParticleMovementFunctionFactory] {
    val name = "SwarmMovementStep"
    val strategy = ep.algorithm match {
      case p: ParticleSwarmParameters => SwarmMovementStep(p.implementation, ep.searchSpace)
      case _ => None
    }
  }

  // indexed factory of swarm movement functions
  implicit def moveSwarmFactory: IndexedParticleMovementFunctionFactory = Factory()
}

object SwarmMovementStep extends LazyLogging {

  // local types for the neighborhood influence
  type Influence = Position
  type NeighborhoodInfluence = NeighborhoodInfluenceFunction[Position]

  object Steps {

    type VelocityUpdateFunction = (Particle, Influence) => Velocity
    type PositionUpdateFunction = (Particle, Velocity) => Particle

    /** VelocityUpdateStep -------------------------------------------------------------------------- */

    object VelocityUpdateStep {

      object Strategies {

        @SerialVersionUID(0L)
        object StandardPSO extends Serializable {

          def apply(w: Double, c1: Double, c2: Double): VelocityUpdateFunction = { (p:Particle, influence:Influence) =>
            // generate two random vectors of D uniformly distributed random numbers in [0,1)
            val r1 = reals.sample(p.element.size).toVector
            val r2 = reals.sample(p.element.size).toVector
            // calculate the components of the velocity
            val inertial = p.velocity.map(_ * w)
            val cognitive = (p.element, p.best.element, r1).zipped.map((x, b, r) => c1 * r * (b - x))
            // standard PSO uses the best particle (min function) in the neighborhood including the self
            // the influence is more general, allowing any neighborhood influence function with or without including the self
            val social = (p.element, influence, r2).zipped.map((x, g, r) => c2 * r * (g - x))
            // calculate the new velocity
            val v = (inertial, cognitive, social).zipped.map((i, c, s) => i + c + s)
            logger.whenTraceEnabled({
              logger.trace("Velocity update {")
              logger.trace(s"  Inertial: ${inertial.mkString(", ")}")
              logger.trace(s"  Random 1: ${r1.mkString(", ")}")
              logger.trace(s"  Distance 1: ${(p.element, p.best.element).zipped.map((x, b) => b - x).mkString(", ")}")
              logger.trace(s"  Cognitive: ${cognitive.mkString(", ")}")
              logger.trace(s"  Random 2: ${r2.mkString(", ")}")
              logger.trace(s"  Neighborhood influence(${p.id}): ${influence}")
              logger.trace(s"  Distance 2: ${(p.element, influence).zipped.map((x, g) => g - x).mkString(", ")}")
              logger.trace(s"  Social: ${social.mkString(", ")}")
              logger.trace(s"  Velocity after: ${v.mkString(", ")}")
              logger.trace("}")
            })
            v
          }

          def apply()(
              implicit properties: PropertiesStore): VelocityUpdateFunction = {

            val (w, c1, c2) =
              (properties(InertiaWeightW).as[Double],
                properties(CognitiveCoefficientC1).as[Double],
                properties(SocialCoefficientC2).as[Double])

            StandardPSO(w, c1, c2)
          }

        } // StandardPSO

        @SerialVersionUID(0L)
        object ConstrictionFactorPSO extends Serializable {

          // the constriction factor velocity update function is the standard multiplied by the constriction factor
          def apply(X: Double)(standard: VelocityUpdateFunction): VelocityUpdateFunction = { (p:Particle, influence:Influence)  =>
            standard(p, influence) map (_ * X)
          }

          def apply()(
              implicit properties: PropertiesStore): VelocityUpdateFunction = {

            val (x, c1, c2) =
              (properties(ConstrictionFactorX).as[Double],
                properties(CognitiveCoefficientC1).as[Double],
                properties(SocialCoefficientC2).as[Double])

            ConstrictionFactorPSO(x)(StandardPSO(1.0, c1, c2))
          }
        } // ConstrictionFactorPSO

        @SerialVersionUID(0L)
        object ConstrictionFactorCondensedPSO extends Serializable {

          def apply(X: Double, cmax: Double): VelocityUpdateFunction = { (p:Particle, influence:Influence) =>
            // calculate the social component of the velocity
            val social = (p.element, influence).zipped.map((x, pm) => cmax * (pm - x))
            // calculate the new velocity
            val v = (p.velocity, social).zipped.map((i, s) => X * (i + s))
            logger.whenTraceEnabled({
              logger.trace("Velocity update {")
              logger.trace(s"  Constriction factor: $X")
              logger.trace(s"  Cmax: $cmax")
              logger.trace(s"  Neighborhood influence(${p.id}): ${influence}")
              logger.trace(s"  Distance: ${(p.element, influence).zipped.map((x, g) => g - x).mkString(", ")}")
              logger.trace(s"  Social: ${social.mkString(", ")}")
              logger.trace(s"  Velocity before: ${p.velocity.mkString(", ")}")
              logger.trace(s"  Velocity after: ${v.mkString(", ")}")
              logger.trace("}")
            })
            v
          }

          def apply(cmax: Double)(
              implicit properties: PropertiesStore): VelocityUpdateFunction =
            ConstrictionFactorCondensedPSO(properties(ConstrictionFactorX).as[Double], cmax)

        } // ConstrictionFactorCondensedPSO

      } // Strategies

      import Strategies._

      def apply(p:PSOVUpdateStrategies)(
          implicit properties: PropertiesStore): VelocityUpdateFunction = p match {
        case std:PSOVUpdateStandard => StandardPSO()
        case cf:PSOVUpdateConstrictionFactor => ConstrictionFactorPSO()
        case PSOVUpdateCondensedConstrictionFactor(cmax) => ConstrictionFactorCondensedPSO(cmax)
      }

    } // VelocityUpdateStep

    /** VelocityBoundStep --------------------------------------------------------------------------- */

    object VelocityBoundStep {

       object Strategies {

         object Default {
           // bound velocity to vmax
           def apply(): VelocityBoundFunction = (v, vmax) =>
             (v, vmax).zipped.map((v, vmax) => BoundFunction.default(v, -vmax, vmax)) // reuse default bounding function
         }

      } // Strategies

      def apply(strategy: Option[ParticleSwarmVelocityLimit]): Option[VelocityBoundFunction] = strategy match {
        case Some(_) => Some(Strategies.Default())
        case _ => None
      }

     } // VelocityBoundStep

    /** PositionUpdateStep --------------------------------------------------------------------- */

    object PositionUpdateStep {

      object Strategies {

        object StandardPSO {
          def apply(): PositionUpdateFunction = { (p: Particle, v: Velocity ) =>
            // the velocity is supposed to have already been updated
            val pos: Position = (p.element, v).zipped.map((x, v) => x + v)
            logger.trace(s"Position after: ${pos.mkString(", ")}")
            Particle(p.id, pos, BAD_POINT, v, p.best) // the new particle (yet to be bounded and evaluated)
          }
        } // StandardPSO

      } // Strategies

      def apply(): PositionUpdateFunction = Strategies.StandardPSO()

    } // PositionUpdateStep

    /** MovementBoundStep --------------------------------------------------------------------------- */

    object MovementBoundStep {

      object Strategies {

        object SearchSpace {
          /**
           * default bound function (taken from https://rpubs.com/Joaquin_AR/475474)
           * bound position to search space limits and reset velocity, if position is out of limits
           */
          def apply(): MovementBoundFunction = { (mov: Movement, low: Bound, up: Bound) =>
            val (bPos: Position, bVel: Velocity) = ((mov._1, mov._2).zipped, low, up).zipped.map({
              case ((p: Point, v: Double), l: Point, u: Point) =>
                if (p < l) (l, 0.0)
                else if (p > u) (u, 0.0)
                else (p, v)
            }).toVector.unzip

            (bPos, bVel)
          }
        }

        def apply(strategy: PSOMoveLimitStrategies): Option[MovementBoundFunction] = strategy match {
          case PSOMoveLimitSearchSpace => Some(SearchSpace())
          case _ => logger.error(s"PSO movement limit strategy unknown or not defined"); None
        }

      } // Strategies

      def apply(strategy: Option[PSOMoveLimitStrategies]): Option[MovementBoundFunction] = strategy match {
        case Some(s) => Strategies(s)
        case _ => None
      }

    } // MovementBoundStep

  } // Steps

  object Implementations {

    import Steps._ // bring steps into context

    /**
     * Structure of the swarm update step in the standard PSO: velocity + position update
     */
    object StandardPSO {

      /** A factory for the configurable functions used in the implementation of the Standard PSO.
        * Grouping the creation of these functions here is a workaround to avoid serialization issues with the swarm
        * in the MWMoveAndEvaluate implementation. As a side effect this implementation also calculates the neighborhood
        * influence only once per swarm, avoiding recalculating it for each particle
        */
      private object Functions {
        def apply(swarm: Swarm, p:PSOTopologyAndStrategies, ssp:SearchSpaceParameters)(
          implicit properties: PropertiesStore): (NeighborhoodInfluence, VelocityUpdateFunction, PositionUpdateFunction) = {

          // build the neighborhood influence function
          def influence: NeighborhoodInfluence = {
            val cmax = p.strategies.velocityUpdate match {
              case std:PSOVUpdateStandard => properties(SocialCoefficientC2).as[Double]
              case cf:PSOVUpdateConstrictionFactor => properties(SocialCoefficientC2).as[Double]
              case PSOVUpdateCondensedConstrictionFactor(cmax) => cmax
            }
            NeighborhoodInfluenceFactory(swarm, p.topology, cmax)
          }

          // build the velocity update function
          def velUpdate: VelocityUpdateFunction = VelocityBoundStep(p.strategies.velocityLimit) match {
            case Some(vf) =>
              val vmax = properties(VelocityLimitVmax).as[Velocity] // the velocity limit
              VelocityUpdateStep(p.strategies.velocityUpdate)(properties)(_, _).bound(vf)(vmax)
            case _ =>
              VelocityUpdateStep(p.strategies.velocityUpdate)(properties)
          }

          // build the particle (position) update function
          def posUpdate: PositionUpdateFunction = MovementBoundStep(p.strategies.movementLimit) match {
            case Some(mf) => PositionUpdateStep()(_, _).bound(mf)(ssp)
            case _ => PositionUpdateStep()
          }

          (influence, velUpdate, posUpdate)
        }
      }

      def apply(p:PSOTopologyAndStrategies, ssp:SearchSpaceParameters)(
        implicit properties: PropertiesStore): ParticleMovementFunctionFactory = swarm => {
        // local instances are stored here to force them to be created only once per swarm and not once for each particle
        // and also to avoid serializing the swarm in the MWMoveAndEvaluate implementation, which causes an exception
        val (influence, velUpdate, posUpdate) = Functions(swarm, p, ssp)

        // return the particle movement function combining velocity and position update
        { p: Particle => posUpdate(p, velUpdate(p, influence(p.id))) }
      }

    } // StandardPSO

    def apply(p: PSOTopologyAndStrategies, ssp: SearchSpaceParameters)(
      implicit properties: PropertiesStore): ParticleMovementFunctionFactory = StandardPSO(p, ssp)

  } // Implementations



  // a function type that works as a "decorator" for a collection of IndexedParticleMovementFunctionFactory functions indexed by position
  type IndexedParticleMovementFunctionFactory = Int => ParticleMovementFunctionFactory

  // implicit conversions between ParticleMovementFunctionFactory and IndexedParticleMovementFunctionFactory
  implicit def toIndexedParticleMovementFunctionFactory(f: ParticleMovementFunctionFactory): IndexedParticleMovementFunctionFactory = _ => f  // single function
  implicit def toIndexedParticleMovementFunctionFactory(vf: ParticleMovementFunctionFactories): IndexedParticleMovementFunctionFactory = i => vf(i)  // a vector of functions

  /**
    * IndexedParticleMovementFunctionFactory is a "decorator" for a collection of [[ParticleMovementFunctionFactory]]
    * functions indexed by position. It abstracts the mapping between island/partition IDs and their corresponding
    * movement function factory.
    */
  object IndexedParticleMovementFunctionFactory {

    /** =IndexedParticleMovementFunctionFactory factory=
      *
      * Creates an indexed movement function factory
      * This factory is specific for implementations with single configurations (i.e. sequential, master-worker).
      *
      * @param p the PSO topology and strategies
      * @param ssp the search space parameters
      * @return a new indexed movement function factory or None
      */
    def apply(p: PSOTopologyAndStrategies, ssp: SearchSpaceParameters)(
      implicit properties: PropertiesStore): IndexedParticleMovementFunctionFactory =
      Implementations(p, ssp)  // implicit conversion to IndexedParticleMovementFunctionFactory

    /** =IndexedParticleMovementFunctionFactory factory=
      *
      * Creates an indexed movement function factory
      * This factory is specific for implementations with multiple configurations (i.e. island-based).
      * It creates an update function for each island and the returned function adds the
      * mapping between island/partition IDs and their movement function factories.
      *
      * @param conf the islands configurations
      * @param ssp the search space parameters
      * @return a new indexed movement function factory or None
      */
    def apply(conf: PSOIslandsConfiguration, ssp: SearchSpaceParameters)(
      implicit properties: PropertiesStore): IndexedParticleMovementFunctionFactory = {
      // collect the island sizes and properties stores (each island have its own store) from the main store
      val isz: IslandSizes = properties(IslandSizes).as[IslandSizes]
      val iproperties: IslandProperties = properties(IslandProperties).as[IslandProperties]
      // create the movement factories for the islands
      // the properties store is left as a parameter for the creation of the initialization functions and then a map on the result
      // is used to complete the creation by calling the initialization function of each island passing its properties store as argument
      FromIslandConfigurationsCreate(isz.size, conf)(_.conf)(c => Implementations(c, ssp)(_:PropertiesStore)).toVector.zipWithIndex map {
        case (f, i) => f(iproperties(i)) }
      // implicit conversion to IndexedParticleMovementFunctionFactory
    }
  } // IndexedParticleMovementFunctionFactory

  /**
   * =SwarmMovementStep factory=
   *
   * Creates a new movement function factory
   * This is the main entry point for building new movement function factories.
   * It supports both implementations with single and multiple configurations (i.e. island-based) by delegation
   * to other factories.
   *
   *  @param p the PSO implementation
   *  @param ssp the search space parameters
   *  @param properties a property store from which to retrieve the algorithm parameters
   *  @return a new indexed movement function factory or None
   */
  def apply(p: ParticleSwarmImplementation, ssp: SearchSpaceParameters)(
    implicit properties: PropertiesStore): Option[IndexedParticleMovementFunctionFactory] = p match {
    case pso: PSOIslands => Some(IndexedParticleMovementFunctionFactory(pso.conf, ssp))
    case pso: PSOWithSingleConfiguration => Some(IndexedParticleMovementFunctionFactory(pso.conf, ssp))
    case _ => None
  }
}
