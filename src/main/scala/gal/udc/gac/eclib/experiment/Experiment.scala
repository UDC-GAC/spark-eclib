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
package gal.udc.gac.eclib.experiment

import com.typesafe.scalalogging._
//import scala.compat.Platform
import scala.concurrent.duration.Duration
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.util._
import gal.udc.gac.eclib.metaheuristics._

/**
 * @author xoan
 */

object Experiment extends LazyLogging {    
  def run(ep: ExperimentParameters)(implicit ec: ExperimentContext): Unit = {
    ec.configureSBeclib(ep) match { // configure the SBeclib library
      case None => logger.error("Can not get a valid configuration from external library")
      case Some(params) => {
        implicit val p = params  // make the experiment parameters implicit
        logger.info(s"$p")
        EvolutionaryMetaheuristic() match { // create and configure the metaheuristic
          case Some(m: Metaheuristic[_, _]) => (1 to p.repetitions).foreach(i => {  // run the metaheuristic the number of times configured in the experiment parameters
            logger.info(s"[[ Repetition: $i/${p.repetitions} ----------------------------------------------------------------------------- ]]")
            logger.info(s"Repetition elapsed time: ${duration { m.run() } toSeconds}s")  // TODO: add support to metaheuristics with I/O arguments
//          Platform.collectGarbage // force garbage collection between runs (intended to minimize GC overload during runs) 
          })
          case None => logger.error("Invalid metaheuristic")
        }
        ec.cleanSBeclib(p) // clean the SBeclib library configuration
      }
    }
  }
}