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
package gal.udc.gac.eclib.libraries

/** @author xoan
 * 
 */
import scala.annotation.meta.field
import sna.Library
import gal.udc.gac.eclib._

@SerialVersionUID(0L)
trait LazyDynamicLibrary extends Serializable {
  val name: String
  @(transient @field) // workaround to load the library in the Spark workers
  protected lazy val lib: Library = Library(name)
  
  /** Configure the external library
   *  
   *  @param p The experiment parameters
   *  @return The new experiment parameters (external library configuration might modify them)
   */
  def configure(p: ExperimentParameters): Option[ExperimentParameters] = Some(p)
  
  /** Cleanup the external library
   *  
   *  @param p The experiment parameters
   *  @return true if the library has been cleaned successfully, false otherwise. Default implementation: true 
   */
  def clean(p: ExperimentParameters) = true
}