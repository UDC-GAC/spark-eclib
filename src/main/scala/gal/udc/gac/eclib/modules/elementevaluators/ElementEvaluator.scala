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
package gal.udc.gac.eclib.modules.elementevaluators

import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.libraries._
import gal.udc.gac.eclib.modules.elementevaluators.benchmarks.BenchmarkFunction
import gal.udc.gac.eclib.modules.{AlgorithmStep, RequiredStrategyFactory}

/** Trait for element evaluation methods (fitness functions)
 *  
 * @author xoan
 */

/*
trait ElementEvaluator extends Serializable {
  
  /** Evaluates an element and returns its fitness
   *  
   *  @param - The element to evaluate
   *  @return The element fitness value
   */
  def evaluate: FitnessFunction
}
*/

@SerialVersionUID(0L)
trait ElementEvaluator extends Serializable { self: AlgorithmStep =>

  @(transient) // do not serialize the factory object
  private object Factory
    extends RequiredStrategyFactory[FitnessFunction] {
    val name = "ElementEvaluator"
    val strategy = ElementEvaluator(ep.evaluationFunction, ec.sbeclib)
  }
  protected implicit val _evaluate: FitnessFunction = Factory() // implicit fitness function
}

/** Companion object used to instantiate new element evaluation functions from their name
 * 
 */
object ElementEvaluator {

  /** Creates a new evaluation function by name
   *
   *  @param name The name of the evaluation function. It may not match the name of the class.
   *  @return A new evaluation function
   */
  def apply(name: String): Option[FitnessFunction] = {
    assert(name.nonEmpty)
    name match {
      case "Test" => Some(TestElementEvaluator())
      case _ => BenchmarkFunction(name)
    }
  }

  /** Accesses an evaluation function located in the SBeclib external library
   *  By now it is used to implement System Biology models: Circadian, Mendes, NFKB, BioPredyn(B1-B6)
   *  Note: don´t use it for B6 unless in sequential mode
   * 
   *  @param name the name of the evaluation function
   *  @param lib the external SBeclib library
   *  @return a new evaluation function
   */
  def apply(name: String, lib: Option[LazyDynamicLibrary]): Option[FitnessFunction] =
    lib match { // TODO: add support to other libraries
      case Some(eclib: SBeclibDynamicLibrary) => SBeclibElementEvaluator(name, eclib) orElse ElementEvaluator(name)
      case _ => ElementEvaluator(name) // try to instantiate a solver without the library
    }
}