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
package gal.udc.gac.eclib.modules.elementevaluators.benchmarks

import gal.udc.gac.eclib.searchspace.{Element, FitnessFunction}

/**
 * The Griewank’s function
 *
 * limits =[-600, 600]
 * best fitness = 0
 */
object Griewank {
  def apply(): FitnessFunction = { e: Element =>
    val sum = e.foldLeft(1, 1.0, 0.0)((acc, x) => // acc = (index, sum1, sum2)
      (acc._1 + 1, acc._2 * math.cos(x / math.sqrt(acc._1)), acc._3 + (x * x)))
    1/4000.0 * sum._3 - sum._2 + 1
  }
}

/** The Schaffer f6 function as in [Clerc&Kennedy,2002]
  *
  * dimensions = 2
  * limits =[-100, 100]
  * best fitness = f(0,0) = 0
  */
object Schaffer_f6 {
  def apply(): FitnessFunction = { e: Element =>
    assert(e.size == 2, s"Wrong dimension in Schaffer_f6, it must be 2")
    val (x, y) = (e(1), e(0))
    0.5 + ((math.pow(math.sin(math.sqrt(x*x + y*y)), 2) - 0.5) / math.pow(1.0 + 0.001*(x*x + y*y), 2))
  }
}

/** The Schaffer_N2 function
  *
  * dimensions = 2
  * limits =[-100, 100]
  * best fitness = f(0,0) = 0
  */
object Schaffer_N2 {
  def apply(): FitnessFunction = { e: Element =>
    assert(e.size == 2, s"Wrong dimension in Schaffer_N2, it must be 2")
    val (x, y) = (e(1), e(0))
    0.5 + ((math.pow(math.sin(x*x - y*y), 2) - 0.5) / math.pow(1.0 + 0.001*(x*x + y*y), 2))
  }
}

/** The Schaffer_N4 function
  *
  * dimensions = 2
  * limits =[-100, 100]
  * best fitness = 0.292579 at [(0,1.25313), (0,-1.25313), (1.25313,0), (-1.25313,0)]
  */
object Schaffer_N4 {
  def apply(): FitnessFunction = { e: Element =>
    assert(e.size == 2, s"Wrong dimension in Schaffer_N4, it must be 2")
    val (x, y) = (e(1), e(0))
    0.5 + ((math.pow(math.cos(math.sin(math.abs(x*x - y*y))), 2) - 0.5) / math.pow(1.0 + 0.001*(x*x + y*y), 2))
  }
}