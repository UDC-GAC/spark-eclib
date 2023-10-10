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
import scala.util.{Try, Success}
import org.apache.spark.{SparkContext, SparkConf}
import gal.udc.gac.eclib.libraries.SBeclibDynamicLibrary
import gal.udc.gac.eclib.ExecutionFrameworkParameters
import gal.udc.gac.eclib.ExperimentParameters
import gal.udc.gac.eclib.ExternalLibrariesParameters
import gal.udc.gac.eclib.SparkParameters

trait SBeclib extends LazyLogging { // TODO: support other external libraries
  val sbeclib: Option[SBeclibDynamicLibrary]  
  
  // Configure the external library
  def configureSBeclib(p: ExperimentParameters): Option[ExperimentParameters] = sbeclib match {
    case Some(lib) => lib.configure(p)
    case None => logger.warn("Failed to get the SBeclib library: skipping configuration"); Some(p)
  }
  
  // Cleanup the external library
  def cleanSBeclib(p: ExperimentParameters) = sbeclib match {
    case Some(lib) => lib.clean(p) 
    case None => true
  }
}

trait ExperimentContext extends SBeclib

case class SequentialExperimentContext(val sbeclib: Option[SBeclibDynamicLibrary]) extends ExperimentContext 

case class SparkExperimentContext(c: SparkConf, val sbeclib: Option[SBeclibDynamicLibrary]) 
  extends SparkContext(c) 
  with ExperimentContext {
  
  // overrides to hide SparkContext log messages when log level = INFO
  override def logInfo(msg: => String) = {} 
  override def logInfo(msg: => String, throwable: Throwable) = {}

  logger.info(s"Running Spark with ${c.getOption("spark.driver.cores").getOrElse("?")} driver cores and" +
    s" ${c.getOption("spark.executor.instances").getOrElse("?")} workers of" +
    s" ${c.getOption("spark.executor.cores").getOrElse("?")} threads")
  logger.debug(s"Spark configuration:\n${c.toDebugString}")
  
  //  def this() = this(new SparkConf(), None, true)

  def this(p: SparkParameters, sbeclib: Option[SBeclibDynamicLibrary]) = this({
      val conf = new SparkConf().setAppName(p.jobName)
      if (p.localRun) conf.setMaster(s"local[${conf.getOption("spark.driver.cores").getOrElse("?")}]")
      else conf.set("spark.locality.wait", "0")}, // force non-local task scheduling without waiting to maximize executor parallelism
      sbeclib)
  
  // Cleanup the external library (driver only)
  override def cleanSBeclib(p: ExperimentParameters) = sbeclib match {
    case Some(lib) => lib.clean(p)
    case None => logger.warn("Failed to clean the SBeclib library"); false
  }
}

// TODO: support more contexts (i.e. MT, Flink, ...)

object ExperimentContext extends LazyLogging {
  def apply(framework: Option[ExecutionFrameworkParameters], libs: Option[ExternalLibrariesParameters]): Option[ExperimentContext] = {
    val sbeclib = if (libs.isDefined) Some(new SBeclibDynamicLibrary(libs.get.sbeclibPath, libs.get.sacessHome)) else None    
    framework match {
      case None => 
        logger.debug("Default experiment context: Sequential")
        Some(new SequentialExperimentContext(sbeclib))  // default: no framework -> sequential code execution
      case Some(p: SparkParameters) => 
        logger.debug(s"$p")
        Some(new SparkExperimentContext(p, sbeclib))
      case _ => None
    }
  }
}