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

import com.typesafe.scalalogging._
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.modules.individualimprovers.IndividualImprover.IndividualImprovementFunction
import gal.udc.gac.eclib.population.Individual

import scala.annotation.meta.field

// functions implemented by the library
trait SBeclibDynamicLibraryFunctions {
  def evaluate: FitnessFunction
  def improve: IndividualImprovementFunction
}

// inherited by classes that need the library to be configured in the workers
trait SBeclibDynamicLibraryConfigurator
  extends SBeclibDynamicLibraryFunctions
    with LazyLogging {
  protected val benchmark: String
  protected val lib: SBeclibDynamicLibrary
    
  @(transient @field) // workaround to load and configure the SBeclib library in the Spark workers
  lazy val configured: Boolean = lib.setup(benchmark)

  override def evaluate: FitnessFunction = { e: Element =>
    if (configured) lib.evaluate(e)
    else {
      logger.error("SBeclib library not configured. Element evaluation returns BAD_POINT.")
      BAD_POINT
    }
  }

  override def improve: IndividualImprovementFunction = { i: Individual =>
    if (configured) lib.improve(i)
    else {
      logger.error("SBeclib library not configured. Skipping local solver.")
      (i, Evaluations.Zero)
    }
  }
}

class SBeclibDynamicLibrary(val name: String, val benchmarksDir: String) 
    extends LazyDynamicLibrary
    with SBeclibDynamicLibraryFunctions
    with LazyLogging {
  
  val params = null // not used (search space parameters are hard-coded in the ECLIB library)
  
  logger.debug(s"Created SBeclibDynamicLibrary from: $name")
  
  
   /** Setup a benchmark in the external library
   *   The implementation cleans the current benchmark if it is different
   *   from that to be setup   
   *   
   *   This method is intended to be used in the workers to setup/clean the library
   *  
   *  @param benchmark The benchmark name
   *  @return true if the benchmark was setup, false otherwise
   */
  private[libraries] def setup(benchmark: String): Boolean = {
    
    def setup(id: Int): Boolean = {
      val bID = lib.SBbench_getID()[Int]                // get current benchmark ID (-1 => no benchmark configured)
      if (bID != id) {
        if (bID >= 0) lib.SBbench_clean(bID)[Int]       // clean current configuration (ignore error conditions)  
        lib.SBbench_setup(id, benchmarksDir)[Int] != 0  // setup benchmark configuration in the native C library
      } else true
    }
    
    val id = getSBBenchmarkId(benchmark)
    if (id >= 0) setup(id) else false
  }


  /** Configure the external library
   *  
   *  This method is intended to be used in the driver to setup the experiment benchmark
   *  and get back the modified experiment parameters
   *  
   *  
   *  @param p The experiment parameters
   *  @return The new experiment parameters (external library configuration might modify them)
   */
  
  override def configure(p: ExperimentParameters): Option[ExperimentParameters] = {
              
    def getDim: Int = {
      val dim = lib.SBbench_get_dimension()[Int]
      dim match {
        case 0 => logger.error(s"SBbench_get_dimension returns 0")
        case _ => logger.debug(s"SBbench_get_dimension returns: $dim")
      }
      dim
    }
    
    def getVTR: Point = {
      val vtr = lib.SBbench_get_VTR()[Double]
      if (vtr == BAD_POINT) logger.error(s"SBbench_get_VTR returns DBL_MAX")
      else logger.debug(s"SBbench_get_VTR returns: $vtr")
      vtr
    }
  
    def getLowerLimits(dim: Int): Option[Bound] = {
      val lower = new Array[Double](dim)
      lib.SBbench_get_lower_limits(lower)[Int] match {
        case 0 => logger.error(s"SBbench_get_lower_limits returns 0"); None
        case _ => logger.debug(s"SBbench_get_lower_limits returns: ${lower.mkString("[", ",", "]")}"); Some(lower.toVector)
      }
    }
    
    def getUpperLimits(dim: Int): Option[Bound]  = {
      val upper = new Array[Double](dim)
      lib.SBbench_get_upper_limits(upper)[Int] match {
        case 0 => logger.error(s"SBbench_get_upper_limits returns 0"); None
        case _ => logger.debug(s"SBbench_get_upper_limits returns: ${upper.mkString("[", ",", "]")}"); Some(upper.toVector)
      }
    }
    
    // Get dim, VTR, lower and upper limits
    def getParameters: (Int, Point, Option[Bound], Option[Bound]) = {
      val dim = getDim
      val vtr = if (dim == 0) BAD_POINT else getVTR 
      val lower = if (vtr == BAD_POINT) None else getLowerLimits(dim)
      val upper = if (lower.isEmpty) None else getUpperLimits(dim)
      (if (vtr == BAD_POINT || lower.isEmpty || upper.isEmpty) 0 else dim, vtr, lower, upper) //  dim = 0 -> error getting parameters
    }

    logger.debug(s"Configuring SBeclibDynamicLibrary($name) for benchmark ${p.evaluationFunction}")
    if (setup(p.evaluationFunction)) { // setup lib and get dim, VTR, lower and upper limits
      val (dim, vtr, lower, upper) = getParameters 
      logger.debug(s"${p.evaluationFunction} parameters: Dim: $dim VTR: $vtr" + 
                   s" Limits: ${lower.mkString("[", ",", "]")} ${upper.mkString("[", ",", "]")}")
      if (dim == 0) None
      else Some(p.copy(
          searchSpace = p.searchSpace.copy(dimensions = dim, lowerLimits = lower, upperLimits = upper), 
          terminationCriteria = p.terminationCriteria.copy(targetValue = 
            math.max(vtr, p.terminationCriteria.targetValue))  // configuration VTR overwrites library VTR
          ))
    } else None
 }
  
  /** Cleanup the external library
   *  
   *  @param p The experiment parameters
   *  @return true if the library has been cleaned successfully, false otherwise
   */
  override def clean(p: ExperimentParameters): Boolean = {
    logger.debug(s"Cleaning SBeclibDynamicLibrary($name) for benchmark ${p.evaluationFunction}")
    val id = getSBBenchmarkId(p.evaluationFunction)
    if (id >= 0) {
      lib.SBbench_clean(id)[Int] match {
        case 0 => logger.warn("SBbench_clean returned 0"); false
        case 1 => true
      } 
    } else false
  }

  /** Evaluates an element and returns its fitness
   *  
   *  @param e The element to evaluate
   *  @return The element fitness value. DBL_MAX for non finite result.
   */
  override def evaluate: FitnessFunction = {e: Element =>
  val fitness = lib.SBbench_evaluate(e.toArray)[Double]
    if (fitness == BAD_POINT) {
      logger.warn("SBbench_evaluate returns DBL_MAX")  // INF and NAN results are wrapped as DBL_MAX
//      logger.warn(s"Element: [$e]")
    }
    fitness
  }
  
  /**
   * Call the Local Solver
   * 
   * @param i the individual to be optimized
   * @return a tuple with a new individual and the number of evaluations made by the LS
   */
  
   override def improve: IndividualImprovementFunction = {
     
      def nl2sol: IndividualImprovementFunction = {i: Individual =>
        // Same default values used by the SaCess library. TODO: add them to the configuration parameters
        val max_evals = 5000
        val iterations = 100
        
        val values = i.element.toArray
        val fitness = new Array[Double](1)
        val evals = lib.sacess_nl2sol(values, fitness, max_evals, iterations)[Long]
        logger.debug(s"NL2SOL -> initial point: ${i.element.mkString("[", ",", "]")} improved: ${values.mkString("[", ",", "]")} initial fitness: ${i.fitness} fitness: ${fitness(0)} evaluations: $evals")
        if (evals == Evaluations.Zero) { logger.warn("sacess_nl2sol returns 0"); (i, Evaluations.Zero) } // evals == 0 => left the individual as it is
        else (new Individual(values.toSeq, fitness(0)), evals) 
      }    
 
/*     
 * TODO: add DHC support -> how to select the local solver? Configure it in the configure method?
     def dhc: IndividualImprovementFunction = (i: Individual) => {
       logger.warn("SB ECLIB DHC local solver not yet supported")
       (i, 0)
     }
*/
      
     nl2sol  // By now only NL2SOL is supported
   }
}
