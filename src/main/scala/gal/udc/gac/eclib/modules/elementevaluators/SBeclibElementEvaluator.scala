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
package gal.udc.gac.eclib.modules.elementevaluators

import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.libraries._

/** Evaluation method using the System Biology library benchmarks (Circadian, Mendes, Nfkb and B1-B6).
 *  
 * @param benchmark The benchmark function to use 
 * @param lib The SBeclib external library
 * 
 * @author xoan
 */

object SBeclibElementEvaluator {

  @SerialVersionUID(0L)
  private class SBeclibConfigurator(val benchmark: String, val lib: SBeclibDynamicLibrary)
    extends FitnessFunction
      with SBeclibDynamicLibraryConfigurator // workaround to load and configure the SBeclib library in the Spark workers
      with Serializable {
     override def apply(e: Element): Double = evaluate(e)
  }

  def apply(benchmark: String, lib: SBeclibDynamicLibrary): Option[FitnessFunction] =
    if (getSBBenchmarkId(benchmark) >= 0) Some(new SBeclibConfigurator(benchmark, lib)) else None
}