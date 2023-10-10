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

package object libraries extends LazyLogging {
  def getSBBenchmarkId(name: String): Int = name match {
    case "Circadian" => 0
    case "Mendes" => 1
    case "Nfkb" => 2
    case "B1" => 3
    case "B2" => 4
    case "B3" => 5
    case "B4" => 6
    case "B5" => 7
    case _ => logger.error("Unknow benchmark in System Biology library"); -1
  }
}