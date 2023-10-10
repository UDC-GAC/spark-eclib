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

import gal.udc.gac.eclib._
import gal.udc.gac.eclib.experiment._
import gal.udc.gac.eclib.population._

object EvolutionaryMetaheuristic {
  
  type EvolutionaryMetaheuristicType = Metaheuristic[Unit, Unit]

  /** Traits to be implemented by specific evolutionary metaheuristics
   *
   *  hooks are defined at each step to allow for code injection of common logic
   *  in metaheuristic implementations like logging or state conversion
   */
  object PopulationGenerator {  // a population generator
    trait Implementation[T] {
      def generate: PopulationGenerator[T]
      // hook methods
      def onPopulationGeneration: EvolutionStep[T]
      // compose all
      private[EvolutionaryMetaheuristic]
      def generationStep: PopulationGenerator[T] =
        generate andThen onPopulationGeneration
    }
  }
  object TerminationConditionFunction {   // a termination condition
    trait Implementation[T <: EvolutionState[_]] {
      implicit def converge: TerminationConditionFunction[T]
      // hook methods
      def onTerminationCondition: T => T
    }
  }
  object EvolutionStep {   // an evolution step
    trait Implementation[T] {
      def evolve: EvolutionStep[T]
      // hook methods
      def onEvolutionIterationStart: EvolutionStep[T]
      def onEvolutionIterationEnd: EvolutionStep[T]
      // compose all
      private[EvolutionaryMetaheuristic]
      def evolutionStep: EvolutionStep[T] =
        onEvolutionIterationStart andThen evolve andThen onEvolutionIterationEnd
    }
  }

  abstract class Algorithm[T <% Accumulable[T]: Default](n: Int)
      extends EvolutionaryMetaheuristicType 
      with PopulationGenerator.Implementation[T]
      with EvolutionStep.Implementation[T]
      with TerminationConditionFunction.Implementation[EvolutionState[T]] {

    // common evolution with hooks inherited by all evolutionary metaheuristics
    override def run(t: Unit): Unit = {

      def evolution: EvolutionState[T] = generationStep(n) evolveUntil evolutionStep

      onTerminationCondition(evolution)
    }

  }


  import particleswarm.ParticleSwarmOptimization

  def apply()(implicit ep: ExperimentParameters, ec: ExperimentContext)/*: Option[EvolutionaryMetaheuristicType]*/ = ep.algorithm match {
    case _: ParticleSwarmParameters => ParticleSwarmOptimization()
    case _ => None
   }
}