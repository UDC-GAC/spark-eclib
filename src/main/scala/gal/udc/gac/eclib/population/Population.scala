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
import scala.concurrent.duration.Duration
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.modules.individualimprovers.IndividualImprover.IndividualImprovementFunction
import gal.udc.gac.eclib.population.Individual.IndividualOrdering
import org.apache.spark.rdd.RDD

/**
 * @author xoan
 */

/**
 * A population of individuals
 *
 * Populations can be in two states: grouped or distributed,
 * that are implemented by subclassing Population
 *
 */
abstract class Population {
  /**
   * Evaluates a population using the given fitness function
   *
   * @param f implicit fitness function
   * @return a new population evaluated with the given fitness function
   */
  def evaluate(implicit f: FitnessFunction): Population

  /**
   * Improve a population using the given improvement function
   *
   * @param f implicit improvement function
   * @return a new population improved with the given improvement function
   */
  def improve(implicit f: IndividualImprovementFunction): (Population, Evaluations)

  /**
   * The best individual in the population
   */
  def best: Individual

  /**
   * The number of individuals in the population
   */
  def size: Int

  /**
   * Evolve a population until a termination condition is satisfied
   *
   * @tparam T the internal part of the evolution state
   * @param evolve an evolution step to evolve the population
   * @param converged an implicit termination condition
   * @return the state after the evolution has finished
   */
  def evolveUntil[T <% Accumulable[T]: Default](evolve: EvolutionStep[T])(
    implicit converged: TerminationConditionFunction[EvolutionState[T]]): EvolutionState[T] =
    population.evolveUntil[T]((this, implicitly[Default[T]].apply(), Duration.Zero))(evolve)
}

object Population {
  /** populations are created grouped by default */
  def apply(): GroupedPopulation = GroupedPopulation()
  def apply(p: Individuals): GroupedPopulation = GroupedPopulation(p)
  def apply(i: Individual*): GroupedPopulation = Population(i.toVector)   // Individual* -> Population

  /**
   * Create a population of n unique individuals
   * @note the population is not evaluated nor improved
   *
   * @param n the size of the population (n > 0)
   * @param g implicit builder function used to generate new elements
   * @return a population of n different individuals
   */
  def apply(n: Int)(implicit g: GenerationFunction): GroupedPopulation = GroupedPopulation(n)
}
      
@SerialVersionUID(0L)
class GroupedPopulation(val individuals: Individuals)
  extends Population
    with Serializable {

  override def toString = s"Population[${individuals.mkString(", ")}]"

  override def evaluate(implicit f: FitnessFunction): GroupedPopulation =
    individuals.map(_ evaluate) // #evaluations = #individuals

  override def improve(implicit f: IndividualImprovementFunction): (GroupedPopulation, Evaluations) = {
    val (population, evals) = individuals.map(_ improve).unzip
    (population, evals.sum)
  }

  /**
   *  population info that is stored lazily to avoid
   *  recalculating it on every call or when it is not used
   */
  private lazy val _best: Individual = individuals.min
  override def best: Individual = _best

  private lazy val _size: Int = individuals.size
  override def size: Int = _size

}

object GroupedPopulation extends LazyLogging {

  def apply(): GroupedPopulation = new GroupedPopulation(Individuals())
  def apply(i: Individuals): GroupedPopulation = new GroupedPopulation(i) // by default populations are initially grouped

  /**
   * Create a grouped population of n unique individuals
   * @note the population is not evaluated nor improved
   *
   * @param n the size of the population (n > 0)
   * @param g implicit builder function used to generate new elements
   * @return a population of n different individuals
   */
  def apply(n: Int)(implicit g: GenerationFunction): GroupedPopulation = {

    @tailrec
    def generateNDistinct(n: Int, p: GroupedPopulation = GroupedPopulation()): GroupedPopulation = {
      logger.trace(s"generateNDistinct - n: $n, size: ${p.size}")
      if (p.size == n) p
      else generateNDistinct(n, (p ++ Seq.fill[Individual](n - p.size)(Individual())).distinct)
    }

    assert(n > 0)
    generateNDistinct(n)
  }

  /**
   * Adds a bound method through an implicit to bound a population to search space limits
   * using a given bound function
   *
   * @param f the bounding function
   * @param params the search space parameters
   * @return a new population bounded to search space limits
   */
  implicit def toBoundedPopulation(p: GroupedPopulation) = new {
    def bound(f: BoundFunction)(params: SearchSpaceParameters): GroupedPopulation =
      if (params.lowerLimits.isEmpty) p
      else p.map(i => i.bound(f)(params))
  }
}

/**
 * A population of individuals that is distributed
 */
abstract class DistributedPopulation
    extends Population {
  def group(): GroupedPopulation
}

object DistributedPopulation {
  def apply(rdd: RDD[Individual]): SparkDistributedPopulation = new SparkDistributedPopulation(rdd)
}

/** Spark implementation */
// TODO: add support to other distributed datasets besides RDD
trait CachedPopulationRDD[T] { self: DistributedPopulation =>

  val rdd: RDD[T] // the population RDD
  implicit protected val ord: Ordering[T] // implicit ordering needed by the RDD min method

  /**
   *  population info that is stored lazily to avoid
   *  recalculating it on every call or when it is not used
   */
  /*  private lazy val _best: T = rdd.min
  override def best: T = _best

  private lazy val _size: Int = rdd.count.toInt
  override def size: Int = _size*/

  override def best: T = rdd.min

  override def size: Int = rdd.count.toInt
}

class SparkDistributedPopulation(override val rdd: RDD[Individual])
  extends DistributedPopulation
    with CachedPopulationRDD[Individual] {

  override final val ord: Ordering[Individual] = IndividualOrdering // override implicit ordering

  import SparkDistributedPopulation.SparkFunctions._

  override def evaluate(implicit f: FitnessFunction): SparkDistributedPopulation =
    rdd.map(i => evaluateIndividual(i))

  override def improve(implicit f: IndividualImprovementFunction): (SparkDistributedPopulation, Evaluations) = {
    val r = rdd.map(i => improveIndividual(i))
    (r.keys, r.values.sum.toLong) // return improved individuals and evaluations as two separate RDDs
  }

  override def group(): GroupedPopulation = rdd.collect
}

object SparkDistributedPopulation {
  // Functions to be distributed to the Spark executors
  object SparkFunctions {
    def evaluateIndividual(i: Individual)(implicit f: FitnessFunction): Individual = i.evaluate
    def improveIndividual(i: Individual)(implicit f: IndividualImprovementFunction): (Individual, Evaluations) = i.improve
  }
}
