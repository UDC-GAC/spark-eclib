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
import gal.udc.gac.eclib.population.TerminationConditionFunction


/** NumberOfStalledGenerationsCondition -------------------------------------------------------- */

trait NumberOfStalledGenerationsCondition { self: AlgorithmStep =>
  def ifNumberOfStalledGenerations: TerminationConditionFunction[Generations] = {
    if (ep.terminationCriteria.stagnation.isDefined)
      NumberOfStalledGenerationsCondition(ep.terminationCriteria.stagnation.get.generations)
    else {g: Generations => false}
  }
}

object NumberOfStalledGenerationsCondition extends LazyLogging {
  def apply(max_stalled_generations: Generations): TerminationConditionFunction[Generations] = { stalled: Generations =>
    if (stalled >= max_stalled_generations) {
      logger.info(s"Termination condition fulfilled: population stagnated after $stalled generations without significant improvement")
      true
    } else false
  }
}

/** StalledGenerationsCounter ------------------------------------------------------------------- */

trait StalledGenerationsCounter
  extends LazyLogging { self: AlgorithmStep =>

  protected case class StalledGenerationsCounterValue(
                                        private var bestFitness: Double = Double.MaxValue,
                                        private var counter: Generations = Generations.Zero) {

    def reset(): Unit = {
      bestFitness = Double.MaxValue
      counter = Generations.Zero
    }

    def value(): Generations = counter

    /**
     * Update the stalled generations counter, if defined
     *
     * @param f the best fitness in the last evolution of the population.
     *          It is declared as a by-name parameter to avoid collecting the best individual when
     *          the stalled generations counter is not used and the population is distributed
     * @return the value of the counter after being updated or Generations.Zero if not used
     */
    def update(f: => Double): Generations = ep.terminationCriteria.stagnation match {
      case Some(p: StagnationParameters) =>
        val fitness = f // force the evaluation of f (to avoid evaluating it more than once)
        if (fitness < bestFitness && Math.abs(bestFitness - fitness) > p.improvement) // if fitness improves more than the improvement tolerance
          counter = Generations.Zero // reset the counter
        else counter = counter + Generations.Unit // else increase the counter
        if (fitness < bestFitness) bestFitness = fitness // if fitness improves best fitness update it
        logger.debug(s"Updating stalled generations counter (counter, best fitness): ${counter}, ${bestFitness}")
        counter // returns the updated counter value
      case _ => Generations.Zero // do nothing
    }
  }

  val stalledGenerationsCounter = StalledGenerationsCounterValue()
}