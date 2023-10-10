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
package gal.udc.gac.eclib.modules.elementgenerators

/**
 * @author Xoán C. Pardo
 **/

import com.typesafe.scalalogging._
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.searchspace.GenerationFunction
import gal.udc.gac.eclib.modules.{AlgorithmStep, RequiredStrategyFactory}

/**
 * Trait to mixin a strategy to generate new elements (vectors in the search space)
 *
 *  @param params the search space parameters
 **/
@SerialVersionUID(0L)
trait ElementGenerator extends Serializable { self: AlgorithmStep =>

  @(transient) // do not serialize the factory object
  private object Factory
    extends RequiredStrategyFactory[GenerationFunction] {
    val name = "ElementGenerator"
    val strategy = ElementGenerator(ep.elementGeneration.method, ep.searchSpace)
  }

  protected implicit val _generate: GenerationFunction = Factory() // implicit element generator function
}

object ElementGenerator extends LazyLogging {

  /**
   *  Creates a new element generator, given its name
   *  @param name the name of the generator to instantiate
   *  @return a new ElementGenerator
   **/
  def apply (name: String, params: SearchSpaceParameters): Option[GenerationFunction] =
    name match {
//    case "Test" => Some(new TestBinaryRandomElementGenerator(params))
//    case "Test" => Some(new TestElementGenerator(params))
    case "Random" => Some(RandomElementGenerator(params))
    case _ => None
  }
}