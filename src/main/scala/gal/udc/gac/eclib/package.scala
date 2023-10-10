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
package gal.udc.gac

package object eclib {

  // TODO??: consider using value classes for Evaluations and Generations
  type Evaluations = Long
  object Evaluations {
    val Zero: Evaluations = 0L
    val Unit: Evaluations = 1L
    val MaxValue: Evaluations = Long.MaxValue
  }

  type Generations = Int
  object Generations {
    val Zero: Generations = 0
    val Unit: Generations = 1
    val MaxValue: Generations = Int.MaxValue
  }
}
