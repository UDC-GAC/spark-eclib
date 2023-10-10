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

import com.typesafe.scalalogging._

import scala.annotation.tailrec
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.modules.individualimprovers.IndividualImprover.IndividualImprovementFunction

// A valid Population does not contain any individual with fitness == BAD_POINT
object ValidPopulation extends LazyLogging {

  /**
   * Creates a grouped population of n different valid individuals
   * @note the population is evaluated
   *       The method filters bad individuals but takes into account all the evaluations performed.
   *
   * @param n the size of the population to be created (n > 0)
   * @param g implicit builder function used to generate new elements
   * @param f implicit fitness function used to evaluate new elements
   * @return a tuple with the new population and the number of evaluations performed
   **/
  def apply(n: Int)(implicit g: GenerationFunction, f: FitnessFunction): (GroupedPopulation, Evaluations) = {
    
    @tailrec
    def generateNDistinct(n: Int, state: (GroupedPopulation, Evaluations) = (GroupedPopulation(), Evaluations.Zero))
        (implicit g: GenerationFunction, f: FitnessFunction): (GroupedPopulation, Evaluations) = {
       val (p, evals) = state
       val sz = p.size
       logger.trace(s"generateNDistinct - n: $n, size: $sz, evals: $evals")
       if (sz == n) state
       else {
         // generate a new population, remove repeated individuals (difference is based only on points, not fitness) and evaluate the rest
         val pp = GroupedPopulation(n - sz).map(_.element).diff(p.map(_.element)).map(Individual(_)).evaluate.asInstanceOf[GroupedPopulation]
         generateNDistinct(n, (p ++ pp.filter(_.isValid), evals + pp.size)) // filter valid individuals and add them to the rest of the population
                                                                            // all the evaluations are summed, even those wasted on bad individuals
       }
    }

    assert(n > 0)
    generateNDistinct(n)
  }

  /**
   * Creates a grouped population of n different valid individuals (best effort version).
   * It tries to create a population of n different valid individuals creating at least min and
   * at most n individuals. Bad individuals are filtered and replaced with new valid individuals only
   * until the min number of individuals is reached. The rest of bad individuals are only filtered.
   * @note the population is evaluated
   *
   * @param n the size of the population to be created (n > 0)
   * @param min the minimum number of valid individuals to create (min > 0 && min <= n)
   * @param g implicit builder function used to generate new elements
   * @param f implicit fitness function used to evaluate new elements
   * @return a tuple with the new population and the number of evaluations performed
   **/
  def apply(n: Int, min: Int)(
      implicit g: GenerationFunction, f: FitnessFunction): (GroupedPopulation, Evaluations) = {
      assert(min > 0 && min <= n)
      val (p, e) = apply(min)  // create min valid individuals (bad individuals are removed and generated again)
      // create and evaluate a population of n - min individuals different from those in p
      val pp = GroupedPopulation(n).map(_.element).diff(p.map(_.element)).take(n - min).map(Individual(_)).evaluate.asInstanceOf[GroupedPopulation]
      (p ++ pp.filter(_.isValid), e + pp.size) // filter valid individuals and add them to the rest of the population
                                               // all the evaluations are summed, even those wasted on bad individuals
    }

  /**
   * Implicit class that allows using the improve method with tuples (GroupedPopulation, Evaluations)
   * @note this class is necessary to use the improve method directly on the result of ValidPopulation
   *
   * @example val (p, e) = ValidPopulation(n).improve
   * @param tuple a tuple (GroupedPopulation, Evaluations)
   * @return a tuple with the population improved and the sum of the evaluations in input tuple
   *         plus the evaluations needed to improve the population
   **/
  implicit class ImprovePopulationWithEvaluations(tuple: (GroupedPopulation, Evaluations)) {
    def improve(implicit i: IndividualImprovementFunction): (GroupedPopulation, Evaluations) = {
      val (p, e) = tuple._1.improve
      (p, tuple._2 + e)
    }
  }
}