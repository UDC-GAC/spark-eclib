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
package gal.udc.gac.eclib.modules.individualimprovers

import gal.udc.gac.eclib._
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.modules.elementgenerators.RandomElementGenerator
import gal.udc.gac.eclib.modules.elementevaluators.TestElementEvaluator
import gal.udc.gac.eclib.population.ValidIndividual

object TestIndividualImprover {

  import IndividualImprover.IndividualImprovementFunction

  /** Random improver
    * @note For testing only.
    *
    * @return A pair with a random individual and 1 evaluation
    */
  def apply(p: SearchSpaceParameters) : IndividualImprovementFunction = {
    implicit val g: GenerationFunction = RandomElementGenerator(p)
    implicit val f: FitnessFunction = TestElementEvaluator()

    { _ => ValidIndividual() }
  }
}