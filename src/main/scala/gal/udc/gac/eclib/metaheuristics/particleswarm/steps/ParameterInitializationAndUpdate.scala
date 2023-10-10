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
import gal.udc.gac.eclib.metaheuristics._
import gal.udc.gac.eclib.metaheuristics.particleswarm._
import gal.udc.gac.eclib.metaheuristics.particleswarm.topology.{SerializableTopology, Topology}
import gal.udc.gac.eclib.metaheuristics.particleswarm.topology.TopologyIO.TopologyConvertOps
import gal.udc.gac.eclib.modules._
import gal.udc.gac.eclib.searchspace.Bound
import gal.udc.gac.eclib.util.Random.reals

import scala.concurrent.duration.Duration

// TODO?: define a general abstraction for parameters and parameter initialization
//  that can be reused for all algorithms and any number, type and method of initialization and dynamic modification?

// trait to mix in initialization and update
trait ParameterInitializationAndUpdate
  extends ParameterInitialization
    with ParameterUpdate {
  self: AlgorithmStep
  with PropertiesStorage =>
}

/** ParameterInitialization --------------------------------------------------------------------- */

trait ParameterInitialization {
  self: AlgorithmStep
    with PropertiesStorage =>

  import ParameterInitialization.ParameterInitializationFunction

  private object Factory
    extends RequiredStrategyFactory[ParameterInitializationFunction] {
    val name = "ParameterInitialization"
    val strategy = ep.algorithm match {
      case p:ParticleSwarmParameters => ParameterInitialization(p.implementation, ep.searchSpace)
      case _ => None
    }
  }

  // parameter initialization function
  def initializeAndStoreAlgorithmParameters: ParameterInitializationFunction = Factory()
}

object ParameterInitialization extends LazyLogging {

  type ParameterInitializationFunction = () => Unit
  @inline private val DoNothing: ParameterInitializationFunction = () => {} // do-nothing function

  object PSOStrategiesParameterInitialization {

    object VelocityUpdateInitialization {

      object PropertyInitialization {

        object InertiaWeightInitialization {
          def apply(w: Double)(
            implicit properties: PropertiesStore): ParameterInitializationFunction = () =>
            properties(InertiaWeightW) = PropertyValue[Double](w)
        }

        object CognitiveCoefficientInitialization {
          def apply(c1: Double)(
            implicit properties: PropertiesStore): ParameterInitializationFunction = () =>
            properties(CognitiveCoefficientC1) = PropertyValue[Double](c1)
        }

        object SocialCoefficientInitialization {
          def apply(c2: Double)(
            implicit properties: PropertiesStore): ParameterInitializationFunction = () =>
            properties(SocialCoefficientC2) = PropertyValue[Double](c2)
        }

        object ConstrictionFactorInitialization {
          def apply(cmax: Double)(
            implicit properties: PropertiesStore): ParameterInitializationFunction = () => {
            // calculate X from cmax: Type 1'' constriction [Clerc and Kennedy, 2002]
            properties(ConstrictionFactorX) =
              PropertyValue[Double](2.0 / math.abs(2.0 - cmax - math.sqrt(math.pow(cmax, 2) - 4.0 * cmax)))
            logger.debug(s"Constriction factor: ${properties(ConstrictionFactorX).as[Double]}")
          }
        }

      } // PropertyInitialization

      import PropertyInitialization._

      object StandardVelocityUpdate {
        def apply(p: PSOVUpdateStandard)(
            implicit properties: PropertiesStore): ParameterInitializationFunction = {

          @inline def w = InertiaWeightInitialization(p.w)
          @inline def c1 = CognitiveCoefficientInitialization(p.c1)
          @inline def c2 = SocialCoefficientInitialization(p.c2)

          () => List(w, c1, c2) foreach( _() )  // return the function that executes the initialization sequence
        }
      } // StandardVelocityUpdate

      object ConstrictionFactorVelocityUpdate {
        def apply(p: PSOVUpdateConstrictionFactor)(
          implicit properties: PropertiesStore): ParameterInitializationFunction = {

          @inline def c1 = CognitiveCoefficientInitialization(p.c1)
          @inline def c2 = SocialCoefficientInitialization(p.c2)
          @inline def X = ConstrictionFactorInitialization(p.c1 + p.c2)

          () => List(c1, c2, X) foreach( _() )  // return the function that executes the initialization sequence
        }
      }

      object CondensedConstrictionFactorVelocityUpdate {
        def apply(p: PSOVUpdateCondensedConstrictionFactor)(
          implicit properties: PropertiesStore): ParameterInitializationFunction =
          ConstrictionFactorInitialization(p.cmax)
      }

      def apply(p: PSOVUpdateStrategies)(
        implicit properties: PropertiesStore): Option[ParameterInitializationFunction] = p match {
        case f: PSOVUpdateStandard => Some(StandardVelocityUpdate(f))
        case f: PSOVUpdateConstrictionFactor => Some(ConstrictionFactorVelocityUpdate(f))
        case f: PSOVUpdateCondensedConstrictionFactor => Some(CondensedConstrictionFactorVelocityUpdate(f))
        case _ => None
      }

    } // VelocityUpdateInitialization

    object VelocityLimitInitialization {

      object Strategies {

        type VelocityLimitInitializationFunction = () => Velocity

        object Constant {
          // expand the vector to dimension size if only the first element is defined
          def apply(vmax: Bound, dim: Int): VelocityLimitInitializationFunction = () =>
            if (vmax.length == dim) vmax else Vector.fill(dim)(vmax.head) // fill the vector with the first element

          def apply(vmax: Bound, ssp: SearchSpaceParameters): Option[VelocityLimitInitializationFunction] = {
            assert(vmax.nonEmpty, "Vmax can not be empty if no velocity limit initialization strategy is provided")
            Some(apply(vmax, ssp.dimensions))
          }
        } // Constant

        object Factor {
          // Vmax = k * (Xmax-Xmin)/2
          def apply(k: Double, lower: Bound, upper: Bound): VelocityLimitInitializationFunction = () =>
            (lower, upper).zipped.map((l, u) => k * (u - l) / 2)

          def apply(s: PSOVMaxInitFactor, ssp: SearchSpaceParameters): Option[VelocityLimitInitializationFunction] = {
            assert(ssp.lowerLimits.isDefined, s"Search Space bounds must be defined to apply the Factor velocity limit initialization strategy")
            Some(apply(s.k, ssp.lowerLimits.get, ssp.upperLimits.get))
          }
        } // Factor

        def apply(strategy: PSOVMaxInitStrategies, ssp: SearchSpaceParameters): Option[VelocityLimitInitializationFunction] = strategy match {
          case s: PSOVMaxInitFactor => Factor(s, ssp)
          case _ => logger.error(s"PSO velocity limit initialization strategy unknown or not defined"); None
        }

        def apply(vc: ParticleSwarmVelocityLimit, ssp: SearchSpaceParameters): Option[VelocityLimitInitializationFunction] = vc.vmaxInitialization match {
          case Some(strategy) => apply(strategy, ssp)
          case None => Constant(vc.vmax, ssp)
        }
      } // Strategies

      def apply(p: Option[ParticleSwarmVelocityLimit], ssp: SearchSpaceParameters)(
          implicit properties: PropertiesStore): Option[ParameterInitializationFunction] = p match {
        case Some(vc) => Strategies(vc, ssp) match {
          case Some(vmax) =>
            Some(() => properties(VelocityLimitVmax) = PropertyValue[Velocity](vmax()))
          case _ => None
        }
        case _ => Some(DoNothing)
      }

    } // VelocityLimitInitialization

    def apply(p: ParticleSwarmStrategies, ssp: SearchSpaceParameters)(
        implicit properties: PropertiesStore): Option[ParameterInitializationFunction] = {

      @inline def vUpdate = VelocityUpdateInitialization(p.velocityUpdate)
      @inline def vLimit = VelocityLimitInitialization(p.velocityLimit, ssp)

      (vUpdate, vLimit) match {
        case (Some(s), Some(t)) => // return the function that runs the initialization functions
          Some(() => List(s, t) foreach ( _() ))
        case _ => None // error => force strategy construction to fail
      }

    }

  } // PSOStrategiesParameterInitialization

  object TopologyInitialization {
    def apply(p: PSOTopologyAndStrategies)(
      implicit properties: PropertiesStore): Option[ParameterInitializationFunction] = Topology(properties(SwarmSize).as[Int], p.topology, properties(SwarmInitialID).as[Int]) match {
        case Some(t) =>
          logger.info(t.asDot(s"${p.topology.shape}"))
          Some(() => // store the topology using the workaround wrapper to support serialization
            properties(SwarmTopology) = PropertyValue[SerializableTopology](SerializableTopology(t)))
        case None => None
      }
  } // TopologyInitialization

  /** =ParameterInitialization factory=
    *
    * Creates a new parameter initialization function.
    * This factory is specific for implementations with single configurations (i.e. sequential, master-worker).
    *
    * @param p          the PSO topology and strategies
    * @param ssp        the search space parameters
    * @param properties the properties store in which the parameters will be stored
    * @return a new parameter initialization function or None
    */
  // TODO: if swarm size < configured size => what happens with topology initialization? Currently the swarm size is used
  def apply(p: PSOTopologyAndStrategies, ssp: SearchSpaceParameters)(
    implicit properties: PropertiesStore): Option[ParameterInitializationFunction] = {

    @inline def strategies = PSOStrategiesParameterInitialization(p.strategies, ssp)
    @inline def topology = TopologyInitialization(p)

    (strategies, topology) match {
      case (Some(s), Some(t)) => // return the function that calls the initialization functions in sequence
        Some(() => List(s, t) foreach (_ ()))
      case _ => None // error in the strategies or topology initialization function => force strategy construction to fail
    }
  }

  /** =ParameterInitialization factory=
    *
    * Creates a new parameter initialization function.
    * This factory is specific for implementations with multiple configurations (i.e. island-based).
    * It creates an initialization function with its own properties store for each island and
    * the returned function calls them in sequence.
    *
    * @param conf       the islands configurations
    * @param ssp        the search space parameters
    * @param properties the properties store in which the parameters will be stored
    * @return a new parameter initialization function or None
    */
  def apply(conf: PSOIslandsConfiguration, ssp: SearchSpaceParameters)(
      implicit properties: PropertiesStore): Option[ParameterInitializationFunction] = {
    // collect the island sizes and properties stores (each island have its own store) from the main store
    val isz: IslandSizes = properties(IslandSizes).as[IslandSizes]
    val iproperties: IslandProperties = properties(IslandProperties).as[IslandProperties]
    // create the initialization functions for the islands
    // the properties store is left as a parameter for the creation of the initialization functions and then a map on the result
    // is used to complete the creation by calling the initialization function of each island passing its properties store as argument
    val funs = FromIslandConfigurationsCreate(isz.size, conf)(_.conf)(c => ParameterInitialization(c, ssp)(_:PropertiesStore)).zipWithIndex map {
      case (f, i) => f(iproperties(i)) }
    // return None if anything has failed or the function that calls the initialization functions of the islands in sequence otherwise
    if (funs.exists(_.isEmpty)) None
    else Some(() => funs foreach ( _.get() ))
  }

  /** =ParameterInitialization factory=
    *
    * Creates a new parameter initialization function.
    * This is the main entry point for building new parameter initialization functions.
    * It supports both implementations with single and multiple configurations (i.e. island-based) by delegation
    * to other factories.
    *
    * @param p          the PSO implementation
    * @param ssp        the search space parameters
    * @param properties the properties store in which the parameters will be stored
    * @return a new parameter initialization function or None
    */
  def apply(p: ParticleSwarmImplementation, ssp: SearchSpaceParameters)(
    implicit properties: PropertiesStore): Option[ParameterInitializationFunction] = p match {
    case pso: PSOIslands => ParameterInitialization(pso.conf, ssp)
    case pso: PSOWithSingleConfiguration => ParameterInitialization(pso.conf, ssp)
    case _ => None
  }
} // ParameterInitialization

/** ParameterUpdate ----------------------------------------------------------------------------- */

/**
 * IMPORTANT: this implementation assumes that the update function is called once per generation (evolution iteration).
 * This is critical for the correct updating of generation-based stored values.
 */

trait ParameterUpdate {
  self: AlgorithmStep
    with PropertiesStorage =>

  import ParameterUpdate.ParameterUpdateFunction

  private object Factory
    extends StrategyFactory[ParameterUpdateFunction] {
    val name = "ParameterUpdate"
    val strategy = ep.algorithm match {
      case p: ParticleSwarmParameters => ParameterUpdate(p.implementation, ep)
      case _ => None
    }
  }

  // parameter update function, if not defined returns the do-nothing function
  def updateStoredAlgorithmParameters: ParameterUpdateFunction = Factory.strategy.getOrElse(ParameterUpdate.DoNothing)
}

object ParameterUpdate extends LazyLogging {

  type ParameterUpdateFunction = State => Unit
  private val DoNothing: ParameterUpdateFunction = _ => {} // do-nothing function

  object PSOStrategiesParameterUpdate {

    object VelocityUpdate {

      object StandardVelocityUpdate {

        object InertiaWeightUpdate {

          // TODO: implement more strategies
          // (e.g. the remaining -10- strategies in the paper: "Inertia Weight Strategies in Particle Swarm Optimization",
          // the Multi-Stage LDIW from the paper: "A Particle Swarm Optimizer with Multi-Stage Linearly-Decreasing Inertia Weight" or
          // other approaches described in the paper: "Major Advances in Particle Swarm Optimization: Theory, Analysis, and Application")

          object Strategies {

            type InertiaWeightUpdateFunction = (Double, State) => Double

            object LinearDecreasingTime {
              def apply(wMin: Double, wMax: Double, tMax: Duration)(
                implicit properties: PropertiesStore): InertiaWeightUpdateFunction = { (_, s) =>
                val t: Duration = properties(AbsoluteElapsedTime).as[Duration] + s._3 // current absolute time
                //          logger.trace(s"w(t+1) = $wMin + ($wMax - $wMin) * ($tMax - $t) / $tMax")
                wMin + (wMax - wMin) * (tMax - t) / tMax
              }
            }

            object LinearDecreasingGenerations {
              def apply(wMin: Double, wMax: Double, gMax: Generations): InertiaWeightUpdateFunction = { (_, s) =>
                val g: Generations = s._2._1 // current generations
                //          logger.trace(s"w(t+1) = $wMin + ($wMax - $wMin) * ($gMax - $g) / $gMax")
                wMin + (wMax - wMin) * (gMax - g) / gMax
              }
            }

            object Random {
              def apply(): InertiaWeightUpdateFunction = { (_, _) =>
                val r = reals.sample()
                //          logger.trace(s"w(t+1) = 0,5 + $r / 2")
                0.5 + r / 2
              }
            }

            private def chaos(z: Double): Double = 4.0 * z * (1 - z) // Chaotic Logistic Mapping (Feng et al., 2007)

            object ChaoticTime {
              def apply(wMin: Double, wMax: Double, tMax: Duration)(
                implicit properties: PropertiesStore): InertiaWeightUpdateFunction = { (_, s) =>
                val t: Duration = properties(AbsoluteElapsedTime).as[Duration] + s._3 // current absolute time
                val z = chaos(reals.sample())
                //          logger.trace(s"w(t+1) = $wMin * $z + ($wMax - $wMin) * ($tMax - $t) / $tMax")
                wMin * z + (wMax - wMin) * (tMax - t) / tMax
              }
            }

            object ChaoticGenerations {
              def apply(wMin: Double, wMax: Double, gMax: Generations): InertiaWeightUpdateFunction = { (_, s) =>
                val g: Generations = s._2._1 // current generations
                val z = chaos(reals.sample())
                //          logger.trace(s"w(t+1) = $wMin * $z + ($wMax - $wMin) * ($gMax - $g) / $gMax")
                wMin * z + (wMax - wMin) * (gMax - g) / gMax
              }
            }

            object ChaoticRandom {
              def apply(): InertiaWeightUpdateFunction = { (_, _) =>
                val z = chaos(reals.sample())
                val r = reals.sample()
                //          logger.trace(s"w(t+1) = 0,5 * $z + $r / 2")
                0.5 * z + r / 2
              }
            }

            def apply(strategy: PSOWAdjustStrategies)(p: PSOVUpdateStandard, ep: ExperimentParameters)(
              implicit properties: PropertiesStore): Option[InertiaWeightUpdateFunction] = strategy match {
              case s: PSOWLinearDecreasingTime =>
                assert(p.w > s.wMin,
                  s"For the Linear Decreasing adjustment strategy, the initial value of the inertia weight must be greater than the final value")
                assert(ep.terminationCriteria.maxTime != Duration.Inf,
                  s"A maximum time must be defined in the termination criteria to adjust the inertia weight using a Linear Decreasing Time strategy")
                Some(LinearDecreasingTime(s.wMin, p.w, ep.terminationCriteria.maxTime))
              case s: PSOWLinearDecreasingGenerations =>
                assert(p.w > s.wMin,
                  s"For the Linear Decreasing adjustment strategy, the initial value of the inertia weight must be greater than the final value")
                assert(ep.terminationCriteria.maxGenerations != Generations.MaxValue,
                  s"A maximum number of generations must be defined in the termination criteria to adjust the inertia weight using a Linear Decreasing Generations strategy")
                Some(LinearDecreasingGenerations(s.wMin, p.w, ep.terminationCriteria.maxGenerations))
              case PSOWRandom => Some(Random())
              case s: PSOWChaoticTime =>
                assert(p.w > s.wMin,
                  s"For the Chaotic adjustment strategy, the initial value of the inertia weight must be greater than the final value")
                assert(ep.terminationCriteria.maxTime != Duration.Inf,
                  s"A maximum time must be defined in the termination criteria to adjust the inertia weight using a Chaotic Time strategy")
                Some(ChaoticTime(s.wMin, p.w, ep.terminationCriteria.maxTime))
              case s: PSOWChaoticGenerations =>
                assert(p.w > s.wMin,
                  s"For the Chaotic adjustment strategy, the initial value of the inertia weight must be greater than the final value")
                assert(ep.terminationCriteria.maxGenerations != Generations.MaxValue,
                  s"A maximum number of generations must be defined in the termination criteria to adjust the inertia weight using a Chaotic Generations strategy")
                Some(ChaoticGenerations(s.wMin, p.w, ep.terminationCriteria.maxGenerations))
              case PSOWChaoticRandom => Some(ChaoticRandom())
              case _ => None
            }

          } // Strategies

          def apply(p: PSOVUpdateStandard, ep: ExperimentParameters)(
            implicit properties: PropertiesStore): Option[ParameterUpdateFunction] = {

            def weightUpdate(strategy: PSOWAdjustStrategies): Option[ParameterUpdateFunction] = Strategies(strategy)(p, ep) match {
              case Some(wu) => Some(state => {
                val current = properties(InertiaWeightW).as[Double] // get current inertial weight stored value
                properties(InertiaWeightW) = PropertyValue[Double](wu(current, state)) // update stored W value
              })
              case None => None
            }

            p.strategies match {
              case Some(s) => s.inertiaWeightAdjustment match {
                case Some(iw) => weightUpdate(iw)
                case None => None
              }
              case None => None
            }
          }

        } // InertiaWeightUpdate

        /*
              // TODO: support dynamic adjustment of cognitive and social coefficients

              object CognitiveCoefficientUpdate {
                def apply(p: PSOVUpdateStandard, ep: ExperimentParameters)(
                    implicit properties: PropertiesStore): ParameterUpdateFunction = state =>
                  properties(CognitiveCoefficientC1) = ???
              }

              object SocialCoefficientUpdate {
                def apply(p: PSOVUpdateStandard, ep: ExperimentParameters)(
                    implicit properties: PropertiesStore): ParameterUpdateFunction = state =>
                  properties(SocialCoefficientC2) = ???
              }
        */

        def apply(p: PSOVUpdateStandard, ep: ExperimentParameters)(
          implicit properties: PropertiesStore): ParameterUpdateFunction = state => {
          InertiaWeightUpdate(p, ep)
          //        CognitiveCoefficientUpdate(p, ep)
          //        SocialCoefficientUpdate(p, ep)
        }
      } // StandardVelocityUpdate

      /*
          // TODO: support dynamic adjustment of constriction factor

          object ConstrictionFactorUpdate {
            def apply(ep: ExperimentParameters)(
                implicit properties: PropertiesStore): ParameterUpdateFunction = state =>
              properties(ConstrictionFactor) = ???
          }

          object ConstrictionFactorVelocityUpdate {
            def apply(p: PSOVUpdateConstrictionFactor, ep: ExperimentParameters)(
                implicit properties: PropertiesStore): ParameterUpdateFunction =
              ConstrictionFactorUpdate(ep)
          }

          object CondensedConstrictionFactorVelocityUpdate {
            def apply(p: PSOVUpdateCondensedConstrictionFactor, ep: ExperimentParameters)(
                implicit properties: PropertiesStore): ParameterUpdateFunction =
              ConstrictionFactorUpdate(ep)
          }
      */

      def apply(p: PSOVUpdateStrategies, ep: ExperimentParameters)(
        implicit properties: PropertiesStore): Option[ParameterUpdateFunction] = p match {
        case f: PSOVUpdateStandard => Some(StandardVelocityUpdate(f, ep))
        //      case f: PSOVUpdateConstrictionFactor => Some(ConstrictionFactorVelocityUpdate(f, ep))
        //      case f: PSOVUpdateCondensedConstrictionFactor => Some(CondensedConstrictionFactorVelocityUpdate(f, ep))
        case _ => None
      }

    } // VelocityUpdate

    object VelocityLimitUpdate {

      object Strategies {

        type VelocityLimitUpdateFunction = (Velocity, State) => Velocity

        // Schutte et al. Linearly Decreasing method (LDVM)
        // IMPORTANT: tt updates both Vmax and w (inertial weight)
        object LDVM {

          // operations to access and modify the values stored in the properties storage by this strategy
          private object StoredValues {

            def lastGlobalBestValue()(
              implicit properties: PropertiesStore): Double = try {
                  properties(LastGlobalBest).as[Double]
                } catch {
                  case _: NoSuchElementException => Double.MaxValue
                }

            def generationsWithoutImprovement()(
              implicit properties: PropertiesStore): Generations = try {
                  properties(GenerationsWithoutImprovement).as[Generations]
                } catch {
                  case _: NoSuchElementException => Generations.Zero
                }

            def setLastBest(last: Double = Double.MaxValue)(
                implicit properties: PropertiesStore): Unit = {
              properties(LastGlobalBest) = PropertyValue[Double](last)
            }

            def resetIterations(iter: Generations = Generations.Zero)(
                implicit properties: PropertiesStore): Unit = {
              properties(GenerationsWithoutImprovement) = PropertyValue[Generations](iter)
            }

            def incrIterations()(
                implicit properties: PropertiesStore): Generations = {
              val newValue = generationsWithoutImprovement + Generations.Unit
              properties(GenerationsWithoutImprovement) = newValue
              newValue  // returning the new value here, avoid accessing twice (write+read) to the store
            }

          } // StoredValues

          // TODO?: in island models, LDVM is using the global best, that is the same for all the islands.
          // Change this to each island storing its own global best? => adding a "local" update step at the end of each island evolution?
          def apply(s: PSOVMaxReduceLDVM)(
              implicit properties: PropertiesStore): VelocityLimitUpdateFunction = (v, state) => {
            val gbest = state._1.best // swarm best
            if (gbest.fitness < StoredValues.lastGlobalBestValue) {
              StoredValues.setLastBest(gbest.fitness)
              StoredValues.resetIterations()
              v
            } else if (StoredValues.incrIterations() >= s.h) { // check for the dynamic delay period fulfilment
              StoredValues.resetIterations()
              properties(InertiaWeightW) = PropertyValue[Double](properties(InertiaWeightW).as[Double] * s.beta) // reduce the inertial weight
              v.map(_ * s.alfa) // reduce vmax
            } else v
          }

        } // LDVM

        def apply(strategy: PSOVMaxReduceStrategies)(vUpdate: PSOVUpdateStrategies)(
            implicit properties: PropertiesStore): Option[VelocityLimitUpdateFunction] = (strategy, vUpdate) match {
          case (s: PSOVMaxReduceLDVM, vu: PSOVUpdateStandard) =>
            assert(vu.strategies.isEmpty || vu.strategies.get.inertiaWeightAdjustment.isEmpty,
              "LDVM already adjusts the inertial weight, it can not be initialized if another inertial weight adjustment strategy is configured")
            Some(LDVM(s))
          case (s: PSOVMaxReduceLDVM, _) =>
            logger.error(s"LDVM adjusts the inertial weight, it can only be used with StandardVelocityUpdate PSO")
            None
          case _ =>
            logger.error(s"PSO velocity limit update strategy unknown or not defined")
            None
        }

        def apply(vc: ParticleSwarmVelocityLimit)(vUpdate: PSOVUpdateStrategies)(
            implicit properties: PropertiesStore): Option[VelocityLimitUpdateFunction] = vc.vmaxReduce match {
          case Some(strategy) => apply(strategy)(vUpdate)
          case None => None
        }

      } // Strategies

      def apply(p: ParticleSwarmStrategies)(
          implicit properties: PropertiesStore): Option[ParameterUpdateFunction] = {

        def vmaxUpdate(strategy: ParticleSwarmVelocityLimit): Option[ParameterUpdateFunction] = Strategies(strategy)(p.velocityUpdate) match {
            case Some(vmu) => Some(state => {
                val current = properties(VelocityLimitVmax).as[Velocity] // get current Vmax stored value
                properties(VelocityLimitVmax) = PropertyValue[Velocity](vmu(current, state)) // update stored Vmax value
              })
            case None => None
          }

        p.velocityLimit match {
          case Some(vc) => vmaxUpdate(vc)
          case None => None
        }
      }

    } // VelocityLimitUpdate

    def apply(p: ParticleSwarmStrategies, ep: ExperimentParameters)(
        implicit properties: PropertiesStore): Option[ParameterUpdateFunction] = {

      // TODO: what if several update functions depends on the same stored values?

      @inline def vUpdate = VelocityUpdate(p.velocityUpdate, ep)
      @inline def vLimit = VelocityLimitUpdate(p)

      (vUpdate, vLimit) match {
        case (Some(vu), Some(vl)) => // return the function that runs the update functions
          Some { state:State => List(vu, vl) foreach ( _(state) ) }
        case _ => None // error => force strategy construction to fail
      }
    }

  } // PSOStrategiesParameterUpdate

/*
  // TODO: support dynamic topologies

  object TopologyUpdate {
    def apply(t: ParticleSwarmTopology, ep: ExperimentParameters)(
        implicit properties: PropertiesStore): Option[ParameterUpdateFunction] = Some(state =>
      properties(SwarmTopology) = PropertyValue[SerializableTopology](???)
    )
  }
*/

/*
  // TODO: support swarms of variable size
  object SwarmSizeUpdate {
    def apply(sz: Int)(
      implicit properties: PropertiesStore): ParameterUpdateFunction = state =>
      properties(SwarmSize) = PropertyValue[Int](???)
  }
*/


  /** =ParameterUpdate factory=
   *
   * Creates a new parameter update function
    * This factory is specific for implementations with single configurations (i.e. sequential, master-worker).
    *
   *  @param p the PSO topology and strategies
   *  @param ep the experiment parameters
   *  @param properties the properties store in which the parameters will be stored
   *  @return a new parameter update function or None
   */
  def apply(p: PSOTopologyAndStrategies, ep: ExperimentParameters)(
      implicit properties: PropertiesStore): Option[ParameterUpdateFunction] =
    PSOStrategiesParameterUpdate(p.strategies, ep)

  /** =ParameterUpdate factory=
    *
    * Creates a new parameter update function
    * This factory is specific for implementations with multiple configurations (i.e. island-based).
    * It creates an update function for each island and the returned function calls them in sequence.
    *
    *  @param conf       the islands configurations
    *  @param ep         the experiment parameters
    *  @param properties the properties store in which the parameters will be stored
    *  @return a new parameter update function or None
    */
  def apply(conf: PSOIslandsConfiguration, ep: ExperimentParameters)(
    implicit properties: PropertiesStore): Option[ParameterUpdateFunction] = {
    // collect the island sizes and properties stores (each island have its own store) from the main store
    val isz: IslandSizes = properties(IslandSizes).as[IslandSizes]
    val iproperties: IslandProperties = properties(IslandProperties).as[IslandProperties]
    // create the update functions for the islands
    // the properties store is left as a parameter for the creation of the initialization functions and then a map on the result
    // is used to complete the creation by calling the initialization function of each island passing its properties store as argument
    val funs = FromIslandConfigurationsCreate(isz.size, conf)(_.conf)(c => ParameterUpdate(c, ep)(_:PropertiesStore)).zipWithIndex map {
      case (f, i) => f(iproperties(i)) }
    // return None if anything has failed or the function that calls the update functions of the islands in sequence otherwise
    if (funs.exists(_.isEmpty)) None
    else Some(state => funs foreach ( _.get(state) ))
  }

  /** =ParameterUpdate factory=
    *
    * Creates a new parameter update function.
    * This is the main entry point for building new parameter update functions.
    * It supports both implementations with single and multiple configurations (i.e. island-based) by delegation
    * to other factories.
    *
    *  @param p the PSO implementation
    *  @param ep the experiment parameters
    *  @param properties the properties store in which the parameters will be stored
    *  @return a new parameter update function or None
    */
  def apply(p: ParticleSwarmImplementation, ep: ExperimentParameters)(
    implicit properties: PropertiesStore): Option[ParameterUpdateFunction] = p match {
    case pso: PSOIslands => ParameterUpdate(pso.conf, ep)
    case pso: PSOWithSingleConfiguration => ParameterUpdate(pso.conf, ep)
    case _ => None
  }

  /*  {
    // @inline def size = SwarmSizeUpdate(???)
    @inline def strategies = PSOStrategiesParameterUpdate(p, ep)
    // @inline def topology = TopologyUpdate(p.topology, ep)

    (strategies, topology) match {
      case (Some(s), Some(t)) => // return the function that runs the update functions
        Some { state: State => List(size, s, t) foreach ( _(state) ) }
      case _ => None // error in the update functions => force strategy construction to fail
    }
  }*/
}