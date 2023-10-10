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
package gal.udc.gac.eclib.population

import scala.annotation.tailrec

import gal.udc.gac.eclib._
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.modules.individualimprovers.IndividualImprover.IndividualImprovementFunction

/**
 * @author xoan
 **/

@SerialVersionUID(0L)
class Individual (val element: Element, val fitness: Double = Double.MaxValue)
  extends Serializable  {

  /**
   * Bound an individual to search space limits using the given bound function
   *
   * @param f the bounding function
   * @param params the search space parameters
   * @return a new individual bounded to search space limits
   **/
  def bound(f: BoundFunction)(params: SearchSpaceParameters): Individual =
    new Individual(element.bound(f)(params), fitness)

  /**
   * Evaluates an individual using the given fitness function
   *
   * @param f implicit fitness function
   * @return a new individual evaluated with the given fitness function
   **/
  def evaluate(implicit f: FitnessFunction): Individual =
    new Individual(element, f(element))

  /**
   * Improve an individual using the given improvement function
   *
   * @param f implicit improvement function
   * @return a new individual improved with the given improvement function
   **/
  def improve(implicit f: IndividualImprovementFunction): (Individual, Evaluations) = f(this)

  // valid individuals are those with fitness != BAD_POINT
  def isValid() = fitness != BAD_POINT

//  override def toString = s"[$fitness]<-(${element.mkString(",")})"
  override def toString = s"[$fitness]"
  
/*  override def equals(that: Any) = that match {
      case f: Individual => f.element == element
      case _ => false
    }*/
}

object Individual {

  /**
   * Creates a new individual from a given element
   *
   * @param e the element
   * @param f the fitness value or Double.MaxValue
   * @return the new individual
   **/
  def apply(e: Element, f: Double = Double.MaxValue): Individual = new Individual(e, f)

  /**
   * Creates a new individual using a generation function (builder)
   * @note the individual is not evaluated
   *
   * @param g the implicit element generation function
   * @return the new individual
   **/
  def apply()(implicit g: GenerationFunction): Individual = new Individual(g())
  
  implicit object IndividualOrdering extends Ordering[Individual] {
    def compare(a: Individual, b: Individual) = a.fitness compare b.fitness
  }
}

// Valid individuals are those with fitness != BAD_POINT
object ValidIndividual {

  /**
   * Creates and evaluates a valid individual using a generation function (builder)
   *
   * @param g implicit builder function used to generate new elements
   * @param f implicit fitness function used to evaluate new elements
   * @return a tuple with the new valid and evaluated individual and the number of evaluations performed
   **/
  def apply()(implicit g: GenerationFunction, f: FitnessFunction): (Individual, Evaluations) = {   

    @tailrec
    def getValidIndividual(e: Evaluations = Evaluations.Zero): (Individual, Evaluations) = {
      val i = Individual().evaluate
      if (i.isValid) (i, e + 1)
      else getValidIndividual(e + 1)
    }
    
    getValidIndividual()
  }  
}

// Individuals with ID
trait Identifier {
  val id: IndividualID = IndividualID.Zero()
}

class IndividualWithID (override val id: IndividualID, element: Element, fitness: Double = Double.MaxValue)
extends Individual(element, fitness) with Identifier {
  // override def toString = s"$id, [$fitness]<-(${element.mkString(",")})"
  override def toString: String = s"$id, [$fitness]"
}

object IndividualWithID {

  def apply(id: IndividualID, element: Element, fitness: Double = Double.MaxValue): IndividualWithID =
    new IndividualWithID(id, element, fitness)

  def apply(id: IndividualID, i: Individual): IndividualWithID =
    new IndividualWithID(id, i.element, i.fitness)

  implicit class IndividualIDOps(i: Individual) {
    def withId(id: IndividualID): IndividualWithID = IndividualWithID(id, i)
  }

}



