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
package gal.udc.gac.eclib.metaheuristics

import com.typesafe.scalalogging.LazyLogging
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.experiment.{ExperimentContext, SequentialExperimentContext, SparkExperimentContext}
import gal.udc.gac.eclib.modules.elementevaluators.ElementEvaluator
import gal.udc.gac.eclib.modules.elementgenerators.ElementGenerator
import gal.udc.gac.eclib.modules.terminationconditions._
import gal.udc.gac.eclib.modules.{AlgorithmTemplate, _}
import gal.udc.gac.eclib.population._
import gal.udc.gac.eclib.searchspace.{FitnessFunction, GenerationFunction}
import gal.udc.gac.eclib.util.duration

import scala.concurrent.duration.Duration

/**
 * EvolutionaryMetaheuristic with EvolutionState = (Population, Generations, Evaluations, Duration)
 **/
object BaseEvolutionaryMetaheuristic {

  /**
   * Internal state kept between iterations
   **/
  object IterationState {

    def apply() = DefaultValue()

    def apply(e: Evaluations) = (Generations.Unit, e)

    def apply(g: Generations, e: Evaluations) = (g, e)

    implicit object DefaultValue extends Default[IterationStateType] {
      override def apply() = (Generations.Unit, Evaluations.Zero)
    }

    implicit class AccumulableIterationState(x: IterationStateType) extends Accumulable[IterationStateType] {
      override def +(that: IterationStateType): IterationStateType = (x._1 + that._1, x._2 + that._2)
    }

  }

  import IterationState._

  abstract class Algorithm(
                            implicit override val ep: ExperimentParameters,
                            override val ec: ExperimentContext
                          )
    extends EvolutionaryMetaheuristic.Algorithm[IterationStateType](ep.populationSize)
      with AlgorithmTemplate // implicit experiment parameters and context
      with ElementGenerator  // implicit population generator function
      with ElementEvaluator  // implicit fitness function


  type State = EvolutionState[IterationStateType]
  object State {
    def apply(): State = EvolutionState[IterationStateType]()
  }

  type GeneratorImplementation = EvolutionaryMetaheuristic.PopulationGenerator.Implementation[IterationStateType]
  type EvolutionImplementation = EvolutionaryMetaheuristic.EvolutionStep.Implementation[IterationStateType]
  type TerminationConditionImplementation = EvolutionaryMetaheuristic.TerminationConditionFunction.Implementation[State]


  object Implementations {

    object Default extends LazyLogging {

      import Implicits._

      /**
       * Base algorithm with default implementations of the population generation, evolution and termination condition steps
       * @note default implementation for most hooks are also provided
       **/

      trait Generator
        extends GeneratorImplementation {
        self: AlgorithmStep
          with ElementGenerator
          with ElementEvaluator
          with StalledGenerationsCounter => // module dependencies

        /**
         * Default population generator
         * sequential and parallel versions are provided in the implementation
         **/
        override def generate: PopulationGenerator[IterationStateType] = Generator.Implementation()

        /**
         * Default hook method implementation to be redefined by subclasses if needed
         *
         * @note - IMPORTANT call super.onPopulationGeneration in the overridden methods
         *       to reuse the stagnation counter implementation
         */
        @inline override def onPopulationGeneration: State => State = {
          case (population, (generations, evals), timeElapsed) =>
            val (_, t) = duration { // Initialize the stalled generations counter
              stalledGenerationsCounter.reset() // reset the counter
              stalledGenerationsCounter.update(population.best.fitness) // needed to store the fitness of the best individual before the first iteration
            }
            (population, (generations, evals), timeElapsed + t)
        }

      } // Generator

      trait Evolution
        extends EvolutionImplementation {
        self: AlgorithmStep
          with PropertiesStorage
          with StalledGenerationsCounter =>

        /**
         * Default hook method implementation to be redefined by subclasses if needed
         */
        @inline override def onEvolutionIterationStart: State => State = { s: State =>
          val (population, (generations, evals), timeElapsed) = s
          val (best, t) = duration { // store absolute time and log state
            // store the absolute elapsed time at the beginning of the evolution for those implementations that need it later
            propertiesStore(AbsoluteElapsedTime) = PropertyValue[Duration](timeElapsed)
            logger.debug(s"Population: $population")
            population.best // included inside the duration block to account for the time spent accessing the best individual
                            // because it can execute a min Spark action if the population is distributed
          }
          logger.info(s"Iteration summary (generations, evaluations, time(s), best): " +
            s"${(generations, evals, timeElapsed, best).mkString}") // the time is logged without adding the time spent in the method
          (population, (generations, evals), t) // return only the time spent in the method (it is accumulated in the general evolution implementation)
        }

        /**
         * Default hook method implementation to be redefined by subclasses if needed
         *
         * @note - IMPORTANT call super.onEvolutionIterationEnd in the overridden methods
         *       to reuse the stagnation counter implementation
         **/
        @inline override def onEvolutionIterationEnd: State => State = {
          case (population, (generations, evals), timeElapsed) =>
            val (_ , t) = duration { // Update the stalled generations counter
              stalledGenerationsCounter.update(population.best.fitness) // fitness of the best individual in this iteration
            }
            // the default value (Generations.Unit) to update the number of generations after the evolution is returned here
            // the current number of generations is stored in the generations field of the state during the evolution
            (population, (Generations.Unit, evals), timeElapsed + t)
        }

      } // Evolution

      trait TerminationCondition
        extends TerminationConditionImplementation
          with NumberOfGenerationsCondition
          with NumberOfEvaluationsCondition
          with ByNameValueToReachCondition  // by-name workaround to avoid collecting the best individual when
                                            // the VTR condition is not used and the population is distributed
          with ElapsedExecutionTimeCondition
          with NumberOfStalledGenerationsCondition {
        self: AlgorithmStep
          with StalledGenerationsCounter =>

        /**
         * Default termination condition
         *
         * It checks for a fitness value to reach, the number of generations and the number of stalled generations,
         * the number of evaluations and the execution time
         *
         * @note the time spent here is not included as part of the iteration duration,
         *       but is accounted for in the total evolution duration
         */
        override implicit def converge = {
          case (population, (generations, evals), timeElapsed) =>
            logger.debug(s"TIME (converge): $timeElapsed")
            ifValueToReach(population.best.fitness) ||
              ifNumberOfGenerations(generations) ||
              ifNumberOfEvaluations(evals) ||
              ifExecutionTime(timeElapsed) ||
              ifNumberOfStalledGenerations(stalledGenerationsCounter.value)
        }

        // default hook logging method implementation to be redefined by subclasses if needed
        @inline override def onTerminationCondition: State => State = { // log final state
          case (population, (generations, evals), timeElapsed) =>
            val (best, t) = duration {
              logger.debug(s"Population: $population")
              population.best // included here because it can execute a min Spark action if the population is distributed
            }
            val msg = s"summary (generations, evaluations, time(s), best): ${(generations, evals, timeElapsed + t, best).mkString}"
            logger.info(s"Iteration " + msg)
            logger.info(s"Evolution " + msg)
            (population, (generations, evals), timeElapsed + t)
        }

      } // TerminationCondition

      /** default base algorithm */
      trait Algorithm
        extends BaseEvolutionaryMetaheuristic.Algorithm
          with PropertiesStorage
          with StalledGenerationsCounter
          with Generator
          with Evolution
          with TerminationCondition

      /** Generator implementation */
      object Generator {

        object Implementation {

          object Sequential {
            /**
             * @return a function (sequential version) to generate the initial state of the evolution
             **/
            def apply()(implicit g: GenerationFunction, f: FitnessFunction): PopulationGenerator[IterationStateType] = (n: Int) =>
              duration {
                val (p, e) = ValidPopulation(n) // population of size n (valid and evaluated)
                (p, (Generations.Unit, e))
              }
          }

          object Spark {

            /**
             * @return a function (Spark version) to generate the initial state of the evolution
             **/
            def apply(sc: SparkExperimentContext)(implicit g: GenerationFunction, f: FitnessFunction): PopulationGenerator[IterationStateType] = (n: Int) =>
              duration {
                // distribute the evaluation of the initial population (not evaluated and without duplicates)
                // and filter not valid individuals
                val dp: DistributedPopulation = Population(n).distribute(sc).evaluate.filter(_.isValid()).cache
                if (dp.size < n) logger.warn(s"Proceeding with only ${dp.size} valid individuals, that is less than the population size ($n) required")
                (dp, (Generations.Unit, n: Evaluations)) // all the evaluations are summed, even those wasted on bad individuals
              }

          } // Spark

          def apply()(implicit ep: ExperimentParameters, ec: ExperimentContext, g: GenerationFunction, f: FitnessFunction): PopulationGenerator[IterationStateType] = {
            if (ep.elementGeneration.parallelism == "Sequential") Sequential()
            else ec match {
              case _: SequentialExperimentContext =>
                logger.warn("BaseEvolutionaryMetaheuristic.Generator: parallel option not supported in a Sequential context, defaulting to sequential")
                Sequential()
              case sc: SparkExperimentContext =>
                Spark(sc)
              case _ =>
                logger.warn("BaseEvolutionaryMetaheuristic.Generator: experiment context not supported, defaulting to sequential")
                Sequential()
            }
          }

        } // Implementation

      } // Generator

    } // Default

  } // Implementations

  private object Implicits {

    /**
     * Implicit class to add the method mkString to a tuple containing the main state information
     * @param s the tuple containing the main state information
     * @return a String with the main state information
     * @note I've tried overriding toString instead of defining a new mkString,
     *       but it doesn't work and I don't know exactly why (maybe a double implicit resolution?)
     **/
    implicit class StateToString(s: (Generations, Evaluations, Duration, Individual)) {
      // return a string with the state information
      def mkString(): String = s"${s._1}, ${s._2}, ${s._3.toSeconds}, [${s._4.fitness}]<-(${s._4.element.mkString(",")})"
    }

  } // Implicits

}
