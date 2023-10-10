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
import gal.udc.gac.eclib._

/**
 * @author xoan
 * 
 * Implements a serial executor (computation done in the caller´s thread)
 */

object ExperimentRunner extends LazyLogging {  
  def run(config: EclibConfiguration): Unit = 
    ExperimentContext(config.executionFramework, config.libraries) match { // Configure the experiment context
      case None => logger.error("Invalid experiment context")
      case Some(context) => 
        implicit val ec = context // define implicit experiment context
        config.experiments.foreach(params => Experiment.run(params)) // run the experiments
    }
}