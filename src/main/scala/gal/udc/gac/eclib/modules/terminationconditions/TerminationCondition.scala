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
package gal.udc.gac.eclib.modules.terminationconditions

import com.typesafe.scalalogging.LazyLogging
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.modules.AlgorithmStep
import gal.udc.gac.eclib.population._

import scala.concurrent.duration.Duration

/** NumberOfGenerationsCondition ---------------------------------------------------------------- */

trait NumberOfGenerationsCondition { self: AlgorithmStep =>
  def ifNumberOfGenerations: TerminationConditionFunction[Generations] =
    NumberOfGenerationsCondition(ep.terminationCriteria.maxGenerations)
}

object NumberOfGenerationsCondition extends LazyLogging {
  def apply(max_generations: Generations): TerminationConditionFunction[Generations] = { generations: Generations =>
    if (max_generations != default.maxGenerations && generations >= max_generations) {
      logger.info(s"Termination condition fulfilled: generations $generations >= $max_generations")
      true
    } else false
  }
}

/** NumberOfEvaluationsCondition --------------------------------------------------------------- */

trait NumberOfEvaluationsCondition { self: AlgorithmStep =>
  def ifNumberOfEvaluations: TerminationConditionFunction[Evaluations] =
    NumberOfEvaluationsCondition(ep.terminationCriteria.maxEvaluations)
}

object NumberOfEvaluationsCondition extends LazyLogging {
  def apply(max_evals: Evaluations): TerminationConditionFunction[Evaluations] = { evals: Evaluations =>
    if (max_evals != default.maxEvaluations && evals >= max_evals) {
      logger.info(s"Termination condition fulfilled: evaluations $evals >= $max_evals")
      true
    } else false
  }
}

/** ValueToReachCondition --------------------------------------------------------------- */

/** By-value version of the VTR condition */
trait ValueToReachCondition { self: AlgorithmStep =>
  def ifValueToReach: TerminationConditionFunction[Double] =
    ValueToReachCondition(ep.terminationCriteria.targetValue)
}

/**
 * By-name version of the VTR condition
 *
 * Workaround to avoid collecting the best individual when
 * the VTR condition is not used and the population is distributed
 */
trait ByNameValueToReachCondition { self: AlgorithmStep =>

  import ValueToReachCondition.ByNameTerminationConditionFunction

  def ifValueToReach: ByNameTerminationConditionFunction[Double] =
    ValueToReachCondition.ByName(ep.terminationCriteria.targetValue)
}

object ValueToReachCondition extends LazyLogging {

  /** By-value version of the VTR condition */
  def apply(vtr: Double): TerminationConditionFunction[Double] = (fitness: Double) =>
    if (vtr == default.targetValue) false
    else condition(fitness, vtr)

  /**
   * By-name version of the VTR condition
   *
   * Workaround to avoid collecting the best individual when
   * the VTR condition is not used and the population is distributed
   */
  type ByNameTerminationConditionFunction[T] = (=> T) => Boolean

  object ByName {
    def apply(vtr: Double)(fitness: => Double): Boolean =
      if (vtr == default.targetValue) false
      else condition(fitness, vtr)
  }

  /** common code of the condition for by-name and by-value versions */
  private def condition(fitness: Double, vtr: Double): Boolean =
    if (fitness <= vtr) {
      logger.info(s"Termination condition fulfilled: value to reach (VTR) $fitness <= $vtr")
      true
    } else false
}

/** ElapsedExecutionTimeCondition --------------------------------------------------------------- */

trait ElapsedExecutionTimeCondition { self: AlgorithmStep =>
  def ifExecutionTime: TerminationConditionFunction[Duration] =
    ElapsedExecutionTimeCondition(ep.terminationCriteria.maxTime)
}

object ElapsedExecutionTimeCondition extends LazyLogging {
  def apply(tmax: Duration): TerminationConditionFunction[Duration] = { tnow: Duration =>
    if (tmax.isFinite && tnow >= tmax) {
      logger.info(s"Termination condition fulfilled: execution time ${tnow.toSeconds}s >= ${tmax.toSeconds}s")
      true
    } else false
  }
}



