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

import com.typesafe.scalalogging._
import gal.udc.gac.eclib.experiment.SparkExperimentContext
import gal.udc.gac.eclib.searchspace.GenerationFunction

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import gal.udc.gac.eclib.util._
import org.apache.spark.rdd.RDD

package object population extends LazyLogging {

  type IndividualID = Int
  object IndividualID {
    def Zero() = 0
  }

  type Individuals = Vector[Individual]
  object Individuals {
    def apply(): Individuals = Vector[Individual]()
  }

  implicit def toPopulation(a: Array[Individual]): GroupedPopulation = GroupedPopulation(a.toVector)
  implicit def toPopulation(i: Iterator[Individual]): GroupedPopulation = GroupedPopulation(i.toVector)

  implicit def toIndividuals(p: GroupedPopulation): Individuals = p.individuals
  implicit def toPopulation(i: Individuals): GroupedPopulation = GroupedPopulation(i)

  implicit def toRDD(p: SparkDistributedPopulation): RDD[Individual] = p.rdd
  implicit def toPopulation(rdd: RDD[Individual]): SparkDistributedPopulation = DistributedPopulation(rdd)

  implicit class GroupedtoDistributedPopulation(p: GroupedPopulation) {
    // TODO: support other execution contexts besides Spark
    def distribute(sc: SparkExperimentContext): SparkDistributedPopulation =
      DistributedPopulation(sc.parallelize(p))
    def distribute(sc: SparkExperimentContext, numSlices: Int): SparkDistributedPopulation =
      DistributedPopulation(sc.parallelize(p, numSlices))
  }

  trait Default[T] { def apply(): T }        // used to initialize evolution state with a default value
  trait Accumulable[T] { def +(that: T): T } // used to accumulate evolution state
  type TerminationConditionFunction[T] = (T) => Boolean // used to test evolution convergence

  // generic population evolution function
  type EvolutionState[T] = (Population, T, Duration)
  type EvolutionStep[T] = EvolutionState[T] => EvolutionState[T]
  
  object EvolutionState {
    def apply[T <% Accumulable[T]: Default](): EvolutionState[T] = 
      (Population(), implicitly[Default[T]].apply(), Duration.Zero) 
    def apply[T <% Accumulable[T]: Default](p: Population): EvolutionState[T] = 
      (p, implicitly[Default[T]].apply(), Duration.Zero) 
    def apply[T <% Accumulable[T]](p: Population, t: T): EvolutionState[T] = 
      (p, t, Duration.Zero) 
    def apply[T <% Accumulable[T]](p: Population, t: T, d: Duration): EvolutionState[T] = 
      (p, t, d)
  }

  implicit def toEvolutionState[T](t: ((Population, T), Duration)): (Population, T, Duration) = 
    (t._1._1, t._1._2, t._2)

/*  implicit class EvolutionStateOps[T <% Accumulable[T]](state: EvolutionState[T]) {
    def incr(d: Duration): EvolutionState[T] = (state._1, state._2, state._3 + d) // increments the time component of the state with a given duration
    def incr(inner: T): EvolutionState[T] = (state._1, state._2 + inner, state._3) // increments the inner component of the state with a given inner value
  }*/

  /** Adding evolveUntil to EvolutionState
   *
   *  @note - workaround for Scala 2.12.x - because Tuple3 is now a final class and cannot be
   *        extended, the Evolutionable trait has been replaced by an implicit class. In the end,
   *        this solution turned out to be much more elegant than the original one :)
   */
  implicit class EvolutionStateAsEvolutionable[T <% Accumulable[T]](state: EvolutionState[T]) {
    def evolveUntil(evolve: EvolutionStep[T])(
      implicit converged: TerminationConditionFunction[EvolutionState[T]]): EvolutionState[T] =
      population.evolveUntil(state)(evolve) // call to the general evolution function defined in the package object population
  }

  implicit class AccumulableEvolutionState[T <% Accumulable[T]](p: EvolutionState[T]) 
      extends Accumulable[EvolutionState[T]] {
    override def +(that: EvolutionState[T]): EvolutionState[T] =
      (that._1, p._2 + that._2, p._3 + that._3)
  }
  
  type PopulationGenerator[T] = (Int) => EvolutionState[T] // to implement complex population generators
  object PopulationGenerator {
    object Default {
      def apply[T: Default](n: Int)(implicit g: GenerationFunction): EvolutionState[T] = duration {
        val p: Population = Population(n) // explicit typing to disambiguate Population apply
        (p, implicitly[Default[T]].apply())
      } 
    }
  }

  def evolveUntil[T <% Accumulable[T]](
      initial: EvolutionState[T])(
      evolve: EvolutionStep[T])(
        implicit converged: TerminationConditionFunction[EvolutionState[T]]): EvolutionState[T] = {

    @tailrec
    def evolveUntil_(i: Int = 1, state: EvolutionState[T] = initial): EvolutionState[T] = {
      logger.debug(s"[[ Evolving population: iteration $i <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ]]")
      // check termination condition and update state accounting for the time spent checking the condition
      val (condition, t) = duration { converged(state) }
      val ns = (state._1, state._2, state._3 + t)
      logger.debug(s"TIME (evolveUntil: converged(state)): ${state._3} + $t = ${state._3 + t}")
      // stop or evolve
      if (condition) { logger.debug(s"Population evolved for $i iterations"); ns }
      else evolveUntil_(i + 1, ns + evolve(ns))
    }

    evolveUntil_()
  }
}

