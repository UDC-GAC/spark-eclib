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

import gal.udc.gac.eclib._
import gal.udc.gac.eclib.experiment.{ExperimentContext, SparkExperimentContext}
import gal.udc.gac.eclib.metaheuristics.particleswarm.Velocity
import gal.udc.gac.eclib.modules.elementgenerators.RandomElementGenerator
import gal.udc.gac.eclib.modules.{AlgorithmStep, RequiredStrategyFactory}

trait VelocityInitialization { self: AlgorithmStep =>

  import VelocityInitialization.IndexedVelocityInitializationFunction

  private object Factory
    extends RequiredStrategyFactory[IndexedVelocityInitializationFunction] {
    val name = "VelocityInitialization"
    val strategy = ep.algorithm match {
      case p: ParticleSwarmParameters => VelocityInitialization(p.implementation, ep.searchSpace)
      case _ => None
    }
  }

  // implicit indexed velocity initialization function
  implicit def velocityInitialization: IndexedVelocityInitializationFunction = Factory()
}

object VelocityInitialization {

  type VelocityInitializationFunction = () => Velocity
  type VelocityInitializationFunctions = Vector[VelocityInitializationFunction]

  /**
    * VelocityInitializationFunction abstracts the velocity initialization strategies
    */
  object VelocityInitializationFunction {

    object Strategies {

      // velocity is initially zero
      object Zero {
        def apply(dim: Int): VelocityInitializationFunction = () => Velocity.Zero(dim)
      }

      // velocity is initially a random value bounded to the search space limits
      object Bounded {
        // reuse random element generator to generate a random velocity vector
        private def randomVelocityGenerator(ssp: SearchSpaceParameters): Velocity = RandomElementGenerator(ssp)()
        // velocity bounds are calculated from the search space limits using a reduction factor
        def apply(factor: Double, ssp: SearchSpaceParameters): VelocityInitializationFunction = () => {
          val v = if (factor == 0.0) Velocity.Zero(ssp.dimensions) else randomVelocityGenerator(ssp)
          if (factor > 0.0 && factor < 1.0) v.map(_ * factor) else v
        }
      }

    } // Strategies

    import Strategies._

    def apply(vi: PSOVInitStrategies, ssp: SearchSpaceParameters): Option[VelocityInitializationFunction] = vi match {
      case PSOVInitZero => Some(Zero(ssp.dimensions))
      case PSOVInitBounded(factor) => if (ssp.lowerLimits.isDefined) Some(Bounded(factor, ssp)) else None
      case _ => None
    }

  } // VelocityInitializationFunction

  // a function type for a "decorator" for a collection of [[VelocityInitializationFunctions]] functions indexed by position
  type IndexedVelocityInitializationFunction = Int => VelocityInitializationFunction

  // implicit conversions between VelocityInitializationFunction and IndexedVelocityInitializationFunction
  implicit def toIndexedVelocityInitializationFunction(f: VelocityInitializationFunction): IndexedVelocityInitializationFunction = _ => f  // single function
  implicit def toIndexedVelocityInitializationFunction(vf: VelocityInitializationFunctions): IndexedVelocityInitializationFunction = i => vf(i)  // a vector of functions

  /**
    * IndexedVelocityInitializationFunction is a "decorator" for a collection of [[VelocityInitializationFunctions]]
    * functions indexed by position. It abstracts the mapping between island/partition IDs and their corresponding
    * velocity initialization functions.
    */
  object IndexedVelocityInitializationFunction {

    /** =IndexedVelocityInitializationFunction factory=
      *
      * Creates an indexed velocity initialization function
      * This factory is specific for implementations with single configurations (i.e. sequential, master-worker).
      *
      * @param vi the velocity initialization strategy
      * @param ssp the search space parameters
      * @return a new indexed velocity initialization function or None
      */
    def apply(vi: PSOVInitStrategies, ssp: SearchSpaceParameters): Option[IndexedVelocityInitializationFunction] =
      VelocityInitializationFunction(vi, ssp) match {
        case Some(f) => Some(f) // implicit conversion to IndexedVelocityInitializationFunction
        case None => None
      }

    /** =IndexedVelocityInitializationFunction factory=
      *
      * Creates an indexed velocity initialization function
      * This factory is specific for implementations with multiple configurations (i.e. island-based).
      * It creates a velocity initialization function for each island and the returned function adds the
      * mapping between island/partition IDs and their velocity initialization functions.
      *
      *  @param islands the number of partitions/islands
      *  @param conf the islands configurations
      *  @param ssp the search space parameters
      *  @return a new indexed velocity initialization function or None
      */
    def apply(islands: Int, conf: PSOIslandsConfiguration, ssp: SearchSpaceParameters): Option[IndexedVelocityInitializationFunction] = {
      // check that at least one island is configured
      // it is checked here because is the first place where conf.islands is used in steps
      require(conf.islands.nonEmpty, "At least one island must be configured")
      // create the velocity initialization functions for the islands
      val funs = FromIslandConfigurationsCreate(islands, conf)(_.conf.strategies.velocityInitialization)(VelocityInitializationFunction(_, ssp))
      // return None if anything has failed or the indexed velocity initialization factory taking duplicates into account otherwise
      if (funs.exists(_.isEmpty)) None
      else Some(funs.map(_.get)) // implicit conversion to IndexedVelocityInitializationFunction
    }

  } // IndexedVelocityInitializationFunction

  /** =VelocityInitialization factory=
    *
    * Creates an indexed velocity initialization function
    * This is the main entry point for building new indexed velocity initialization functions.
    * It supports both implementations with single and multiple configurations (i.e. island-based) by delegation
    * to [[IndexedVelocityInitializationFunction]].
    *
    *  @param p the PSO implementation
    *  @param ssp the search space parameters
    *  @param ec the experiment context
    *  @return a new indexed velocity initialization function or None
    */
  def apply(p: ParticleSwarmImplementation, ssp: SearchSpaceParameters)(
    implicit ec: ExperimentContext): Option[IndexedVelocityInitializationFunction] = (p, ec) match {
    case (pso: PSOIslands, sc: SparkExperimentContext) =>
      IndexedVelocityInitializationFunction(sc.defaultParallelism, pso.conf, ssp) // default parallelism = number of islands/partitions
    case (pso: PSOWithSingleConfiguration, _) =>
      IndexedVelocityInitializationFunction(pso.conf.strategies.velocityInitialization, ssp)
    case _ => None
  }
}
