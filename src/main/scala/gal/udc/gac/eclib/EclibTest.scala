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
import scala.util.{Try, Success, Failure}
import gal.udc.gac.eclib.experiment.ExperimentRunner

object EclibTest extends LazyLogging {

  private def usage() = logger.info("Too many arguments. Use: AppName [<pathTo/configFile>]")

  // get first argument or None
  private def getFirstArg(args: Array[String]): Option[String] = Try(args(0)) match {
    case Success(arg) => Some(arg)
    case Failure(_) => None
  }

  def main(args: Array[String]): Unit =
    if (args.length > 1) usage()
    else EclibConfiguration(getFirstArg(args)) match { // load the configuration file
      case Some(conf) => ExperimentRunner.run(conf) // run experiments
      case None => // nothing to do here
    }

}

