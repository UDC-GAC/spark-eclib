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
package gal.udc.gac.eclib

import gal.udc.gac.eclib.population._

package object metaheuristics {

  type SolutionSet = GroupedPopulation
  object SolutionSet {
    def apply(i: Individual*): SolutionSet = new GroupedPopulation(i.toVector)   // Individual* -> SolutionSet
    def apply(v: Individuals): SolutionSet = GroupedPopulation(v)   // Seq[Individual] -> SolutionSet
  }

//  implicit def toSolutionSet(s: Seq[Individual]): SolutionSet = SolutionSet(s.toVector)
//  implicit def toSolutionSet(v: Vector[Individual]): SolutionSet = SolutionSet(v)

  /**
   *  Type definitions for the default iteration state
   **/
  type IterationStateType = (Generations, Evaluations) // default iteration internal state
  type State = EvolutionState[IterationStateType]      // default iteration state
  type EvolutionStepFunction = EvolutionStep[IterationStateType]
  // Factory to create EvolutionStepFunctions by index (used in island-based implementations)
  type IndexedEvolutionStepFunctionFactory = Int => EvolutionStepFunction

  /** =Types and implementations reused from [[BaseEvolutionaryMetaheuristic]]= */

  /** The base algorithm structure that also provides default implementations
   *  of the population generation and termination condition steps
   */
  type BaseAlgorithm = BaseEvolutionaryMetaheuristic.Implementations.Default.Algorithm

  /** the definition of the algorithm steps to be implemented by specific metaheuristics */
  type GeneratorImplementation = BaseEvolutionaryMetaheuristic.GeneratorImplementation
  type EvolutionImplementation = BaseEvolutionaryMetaheuristic.EvolutionImplementation
  type TerminationConditionImplementation = BaseEvolutionaryMetaheuristic.TerminationConditionImplementation
}