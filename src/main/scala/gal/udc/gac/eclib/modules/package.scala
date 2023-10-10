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

import gal.udc.gac.eclib.experiment.ExperimentContext

import scala.collection.mutable

package object modules {

  // Base trait of the algorithm of a metaheuristic. Algorithm steps are used to compose strategies into metaheuristics
  // (defines implicits to bring experiment options into context)
  trait AlgorithmTemplate {
    implicit val ep: ExperimentParameters
    implicit val ec: ExperimentContext
  }
  type AlgorithmStep = AlgorithmTemplate

  /** Base traits for Strategy factories
   *
   * Used inside AlgorithmSteps to instantiate the specific instance of a generic Strategy.
   * The specific instance is usually specified in the configuration parameters and
   * is instantiated by calling constructors defined in the strategies companion objects
   *
   * @tparam T the function type of the strategy
   */
  trait StrategyFactory[T] {
    val name: String                // the strategy name
    val strategy: Option[T]         // the strategy function or None (some strategies are optional)
    def apply(): T = strategy.get   // apply method to be mixed in the private Factory object of each Strategy
  }

  /** Adds a require condition to the base [[StrategyFactory]] for mandatory stategies.
   *
   * @requirement - the strategy has to be defined
   */
  trait RequiredStrategyFactory[T] extends StrategyFactory[T] {
    override def apply(): T = {
      require(strategy.isDefined, s"Failed to create $name: configuration not supported")
      super.apply()
    }
  }

  /**
   * A storage of properties
   *
   * Used to store those values that is necessary to keep between iterations or during an evolution
   * For example, it is used in island models to store the absolute time elapsed at the beginning
   * of an iteration, to be used later by each island when checking for the termination condition
   */

  trait PropertyName
  case object AbsoluteElapsedTime extends PropertyName

  type PropertyValue[T] = T
  object PropertyValue {
    def apply[T](value: T): PropertyValue[T] = value
  }
  implicit class PropertyValueOps(value: PropertyValue[_]) {
    def as[T]: T = value.asInstanceOf[T]
  }

  type PropertiesStore = mutable.Map[PropertyName, PropertyValue[_]]
  object PropertiesStore {
    def apply(): PropertiesStore = mutable.Map[PropertyName, PropertyValue[_]]()
    def apply(elems: (PropertyName, PropertyValue[_])*): PropertiesStore = apply() ++ elems
  }

  trait PropertiesStorage {
    implicit val propertiesStore = PropertiesStore()
  }

}
