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

import java.io.File
import scala.concurrent.duration._
import com.typesafe.scalalogging._
import pureconfig._
import pureconfig.generic._
import pureconfig.configurable._
import pureconfig.ConvertHelpers._
/** import required by pureconfig
 *
 *  Although IntelliJ marks it as unused
 *  DON'T REMOVE IT
 */
import pureconfig.generic.auto._
/** --- */

import gal.udc.gac.eclib.searchspace._

/**
 * Base trait for all parameters
 **/
sealed trait EclibParameters {
  def check(): Boolean = true // check that parameter values are as expected
  def postProcess(): EclibParameters = this // default implementation does nothing (returns this unmodified)
}

/** TerminationCriteria ------------------------------------------------------------------------- */

case class TerminationCriteria(
                                targetValue: Double = Double.MinValue,
                                maxEvaluations: Evaluations = Evaluations.MaxValue,
                                maxGenerations: Generations = Generations.MaxValue,
                                maxTime: Duration = Duration.Inf,
                                stagnation: Option[StagnationParameters] = None) extends EclibParameters {
  override def toString: String = {
    val header = if (targetValue == Double.MinValue &&
      maxEvaluations == Evaluations.MaxValue &&
      maxGenerations == Generations.MaxValue &&
      maxTime == Duration.Inf &&
      stagnation.isEmpty) "" else s"[Termination criteria]\n"
    val body = (if (targetValue != Double.MinValue) s"  VTR: $targetValue\n" else "") +
      (if (maxEvaluations != Evaluations.MaxValue) s"  Max evaluations: $maxEvaluations\n" else "") +
      (if (maxGenerations != Generations.MaxValue) s"  Max generations: $maxGenerations\n" else "") +
      (if (maxTime.isFinite) s"  Max time: $maxTime\n" else "") +
      stagnation.getOrElse("")
    //      (if (stagnation.isDefined) stagnation.get else "")
    header + body
  }

  override def check(): Boolean =
    maxEvaluations > Evaluations.Zero &&
      maxGenerations > Generations.Zero &&
      maxTime > 0.seconds &&
      (stagnation.isEmpty || stagnation.get.check) &&
      // at least one has a non-default value
      (targetValue != Double.MinValue || maxEvaluations != Evaluations.MaxValue || maxGenerations != Generations.MaxValue || maxTime.isFinite || stagnation.isDefined)
}

case class StagnationParameters(
                                 generations: Generations,
                                 improvement: Double = 0.0) extends EclibParameters {
  override def toString: String = s"  Stagnation (generations, improvement): $generations, $improvement\n"
  override def check(): Boolean = generations > Generations.Zero && improvement >= 0.0
}

/** ElementGenerationParameters ----------------------------------------------------------------- */

//@SerialVersionUID(0L)
case class ElementGenerationParameters(
                                  method: String,
                                  parallelism: String = "Sequential") extends EclibParameters /*with Serializable*/ {
  override def toString: String = s"[Element Generation]\n" +
    s"  Generation method: $method\n" +
    s"  Parallelism: $parallelism\n"

  override def check(): Boolean =
    method.nonEmpty &&
      (parallelism.toLowerCase == "sequential" || parallelism.toLowerCase == "parallel")
}

/** SearchSpaceParameters ---------------------------------------------------------------------- */

//@SerialVersionUID(0L)
case class SearchSpaceParameters(
                                  dimensions: Int,
                                  lowerLimits: Option[Bound] = None,
                                  upperLimits: Option[Bound] = None) extends EclibParameters /*with Serializable*/ {


  // expand the size of lower and upper limits to be the same as dimensions
  private def expandLimit(limit: Option[Bound]): Option[Bound] = limit match {
    case None => None
    case Some(limit) =>
      if (limit.size == dimensions) Some(limit)
      else Some(Vector.fill(dimensions)(limit.head)) // fill the vector with the first element
  }

  // expand limits specified using a condensed syntax
  // (limits with the same value in all positions can be specified by providing one value only)
  override def postProcess() = this.copy(
    lowerLimits = expandLimit(lowerLimits),
    upperLimits = expandLimit(upperLimits))

  override def toString: String = s"[Search Space]\n" +
    s"  Dim: $dimensions\n" +
    s"  Lower limits: ${lowerLimits.getOrElse(Nil).mkString("", ", ", "")}\n" +
    s"  Upper limits: ${upperLimits.getOrElse(Nil).mkString("", ", ", "")}\n"

  override def check(): Boolean =
      dimensions > 0 &&
      lowerLimits.isDefined == upperLimits.isDefined && // both are None or Some
      (lowerLimits.isEmpty || // if defined their sizes are equal to dimensions or 1 (they are automatically extended to the size of dimensions)
        (lowerLimits.get.length == upperLimits.get.length && (lowerLimits.get.length == 1 || lowerLimits.get.length == dimensions)))
}

/** AlgorithmParameters ------------------------------------------------------------------------ */

sealed trait AlgorithmParameters
  extends EclibParameters

object AlgorithmParameters {
  // pureconfig implicits - drop "Parameters" from config key when matching AlgorithmParameters subclasses
  implicit val confHint = new FieldCoproductHint[AlgorithmParameters]("name") {
    override def fieldValue(name: String) = name.dropRight("Parameters".length)
  }
}

/**
 * Particle Swarm Optimization parameters
 */

case class ParticleSwarmParameters(implementation: ParticleSwarmImplementation = PSOSequential())
    extends AlgorithmParameters {
  override def toString: String = s"$implementation"
  override def check(): Boolean = implementation.check
}

sealed trait ParticleSwarmImplementation
    extends EclibParameters {
  override def toString: String =
    s"[Particle Swarm Optimization]\n"
}

case class PSOTopologyAndStrategies(
                                     topology: ParticleSwarmTopology = ParticleSwarmTopology(), // default topology: GBest
                                     strategies: ParticleSwarmStrategies = ParticleSwarmStrategies() // default strategies: zero initial velocity and standard velocitu update function
                                   ) extends EclibParameters {
  override def toString: String =
    s"$topology" +
      strategies

  override def check(): Boolean =
    topology.check &&
      strategies.check
}

// common trait for PSO implementations with a single configuration
trait PSOWithSingleConfiguration {
  val conf: PSOTopologyAndStrategies
}

case class PSOSequential(override val conf: PSOTopologyAndStrategies = PSOTopologyAndStrategies())
  extends PSOWithSingleConfiguration
    with ParticleSwarmImplementation {
  override def toString: String = super.toString +
    "  Implementation: Sequential\n" +
    conf.toString
}

case class PSOMWEvaluateOnly(override val conf: PSOTopologyAndStrategies = PSOTopologyAndStrategies())
  extends PSOWithSingleConfiguration
    with ParticleSwarmImplementation {
  override def toString: String = super.toString +
    s"  Implementation: Master-Worker (evaluate only)\n" +
    conf.toString
}

case class PSOMWMoveAndEvaluate(override val conf: PSOTopologyAndStrategies = PSOTopologyAndStrategies())
  extends PSOWithSingleConfiguration
    with ParticleSwarmImplementation {
  override def toString: String = super.toString +
    s"  Implementation: Master-Worker (move and evaluate)\n" +
    conf.toString
}

case class PSOIslandParameters(
                                amount: Int,  // amount or percentage of islands with this configuration
                                conf: PSOTopologyAndStrategies
                              ) extends EclibParameters {

  override def toString: String =
    s"    {\n" +
      s"      Amount: $amount\n" +
      conf.toString.linesIterator.map("    " + _ + "\n").mkString +
      s"    }\n"

  override def check(): Boolean =
    amount > 0 && conf.check
}

case class PSOIslandsConfiguration(
                                    asPercentage: Boolean = false,  // if true regard amounts as percentages
                                    islands: Vector[PSOIslandParameters]
                                  ) extends EclibParameters {
  override def toString: String =
    if (asPercentage) s"  Islands: [\n${islands.mkString.replaceAll("Amount", "Percentage")}  ]\n"
    else s"  Islands: [\n${islands.mkString}  ]\n"

  override def check(): Boolean = islands.nonEmpty && islands.forall(_.check)
}

case class PSOIslands(
                       localIterations: Int,
                       conf: PSOIslandsConfiguration
                     ) extends ParticleSwarmImplementation {

  override def toString: String = super.toString +
    "  Implementation: Islands\n" +
    s"  Local iterations: $localIterations\n" +
    conf

  override def check(): Boolean =
    localIterations > 0 &&
      conf.check
}

object ParticleSwarmImplementation {
  // pureconfig implicits - drop prefix from config key when matching ParticleSwarmImplementation subclasses
  implicit val confHint = new FieldCoproductHint[ParticleSwarmImplementation]("name") {
    override def fieldValue(name: String) = name.drop("PSO".length)
  }
}

case class ParticleSwarmTopology(
                                  self: Boolean = true,
                                  shape: ParticleSwarmTopologyShape = PSOTopologyGlobal,
                                  neighborhoodInfluence: PSONeighborhoodInfluence = PSONeighborhoodInfluenceBest
                                ) extends EclibParameters {
  override def toString: String = s"  Topology {\n" +
    s"    Self: ${if (!self) "not " else ""}included\n" +
    s"    Shape: $shape\n" +
    s"    Neighborhood influence: $neighborhoodInfluence\n" +
  s"  }\n"
  override def check(): Boolean = shape.check()
}

/**
 * Topology shape represented as a DAG with (particle, particle) edges
 *
 * References: [McNabb et al. 2009][Mendes et al. 2002, 2004][Liu et al. 2016] and pySwarm
 *
 * TODO:
 *  - support custom topologies provided as graphviz .dot files
 *  - generate the graphviz .dot file of a topology
 *  - add other common topologies: pyramid, k-clustered, VonNewman, ...
 *
 *  @Note remember to take a look to graphviz related projects for
 *        a dot DSL and other utilities
 */

sealed trait ParticleSwarmTopologyShape
  extends EclibParameters

case object PSOTopologyPyramid // triangle mesh
    extends ParticleSwarmTopologyShape {
  override def toString: String = "Pyramid"
}

case class PSOTopologyDRing(k: Int = 1)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"DirectedRing($k)"
  override def check(): Boolean = k > 0
}

// also known as lbest topology
case class PSOTopologyRing(k: Int = 1)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"Ring($k)"
  override def check(): Boolean = k > 0
}

// regular topology with small-world shortcut as defined in [Liu et al. 2016]
case class PSOTopologyRegular(k: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"Regular($k)"
  override def check(): Boolean = k > 1
}

// regular random topology
case class PSOTopologyRegularRandom(k: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"RegularRandom($k)"
  override def check(): Boolean = k > 1
}

case class PSOTopologyKClusters(k: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"KClusters($k)"
  override def check(): Boolean = k > 1
}

case class PSOTopologyGRandom(dMin: Int, dMax: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"GRandom($dMin, $dMax)"
  override def check(): Boolean = dMin >= 2 && dMin <= dMax
}

// also known as VonNewman 2D
case class PSOTopologySquare(rows: Int, cols: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"Square($rows, $cols)"
  override def check(): Boolean = rows > 1 && cols > 1
}

// TODO: case class VonNewman(dim: Int, range: Int) extends TopologyParams { override def toString: String = s"VonNewman" } // VonNewman: N-dimension, R-range

/** The following topologies were taken from:
  *
  * https://jgrapht.org/javadoc/org.jgrapht.core/org/jgrapht/generate/package-summary.html
  */
case object PSOTopologyLinear
    extends ParticleSwarmTopologyShape {
  override def toString: String = "Linear"
}

// also known with other names: gbest, star, global, all-to-all
case object PSOTopologyComplete
    extends ParticleSwarmTopologyShape {
  override def toString: String = "Complete"
}

// an alias to optimise the GBest common case
case object PSOTopologyGlobal
  extends ParticleSwarmTopologyShape {
  override def toString: String = "Global"
}

// a central particle connected to all the rest
case object PSOTopologyStar
    extends ParticleSwarmTopologyShape {
  override def toString: String = "Star"
}

case object PSOTopologyWheel
    extends ParticleSwarmTopologyShape {
  override def toString: String = "Wheel"
}

case object PSOTopologyScaleFreeNetwork
    extends ParticleSwarmTopologyShape {
  override def toString: String = "ScaleFreeNetwork"
}

case class PSOTopologyHyperCube(dim: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"HyperCube($dim)"
  override def check(): Boolean = dim > 0
}

// Generalized Petersen graph GP(n,k)
case class PSOTopologyGeneralizedPetersen(k: Int = 1)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"GeneralizedPetersen($k)"
  override def check(): Boolean = k > 0
}

// random graph based on the G(n,M) Erdős–Rényi model
case class PSOTopologyGnmRandom(m: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"GnmRandom($m)"
  override def check(): Boolean = m > 0
}

case class PSOTopologyWindmill(m: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"Windmill($m)"
  override def check(): Boolean = m > 0
}

case class PSOTopologyDutchWindmill(m: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"DutchWindmill($m)"
  override def check(): Boolean = m > 0
}

case class PSOTopologyGrid(rows: Int, cols: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"Grid($rows, $cols)"
  override def check(): Boolean = rows > 1 && cols > 1
}

// random bipartite graph based on the G(n,p) Erdős–Rényi model
case class PSOTopologyGnpRandomBipartite(n1: Int, n2: Int, p: Double)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"GnpRandomBipartite($n1, $n2, $p)"
  override def check(): Boolean = n1 > 0 && n2 > 0 && p >= 0.0 && p <= 1.0
}

// random l-planted partition graph
case class PSOTopologyPlantedPartition(l: Int, p: Double, q: Double)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"PlantedPartition($l, $p, $q)"
  override def check(): Boolean = l > 1 && p >= 0.0 && p <= 1.0 && q >= 0.0 && q <= 1.0
}

case class PSOTopologyKleinbergSmallWorld(p: Int, q: Int, r: Int)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"KleinbergSmallWorld($p, $q, $r)"
  override def check(): Boolean = p >= 1 && q >= 0 && r >= 0
}

// directed scale-free graph as described in [Bollobás et al. 2003]
case class PSOTopologyScaleFree(alfa: Float, gamma: Float, deltaIn: Float, deltaOut: Float)
    extends ParticleSwarmTopologyShape {
  override def toString: String = s"ScaleFree($alfa, $gamma, $deltaIn, $deltaOut)"
  override def check(): Boolean = alfa >= 0.0 && alfa <= 1.0 && gamma >= 0.0 && gamma <= 1.0 && deltaIn >= 0.0 && deltaOut >= 0.0
}

case class PSOTopologyCustom(neighborhoods: Map[Int, Vector[Int]])
  extends ParticleSwarmTopologyShape {
  override def toString: String = "Custom"
  //    s"Custom: ${neighborhoods.map(v => s"${v._1}->${v._2.mkString("[",",","]")}").toVector.sorted.reduce((a, b) => s"$a, $b")}"
  override def check(): Boolean = neighborhoods.nonEmpty
}

case class PSOTopologyCustomFromDotFile(file: String)
  extends ParticleSwarmTopologyShape {
  override def toString: String = s"CustomFromDotFile($file)"
  override def check(): Boolean = file.nonEmpty
}

object ParticleSwarmTopologyShape {
  // pureconfig implicits - drop prefix from config key when matching ParticleSwarmTopologyShape subclasses
  implicit val confHint = new FieldCoproductHint[ParticleSwarmTopologyShape]("name") {
    override def fieldValue(name: String) = name.drop("PSOTopology".length)
  }
  // implicit topology reader. Needed to convert map keys from String to Int
  implicit val topologyReader = genericMapReader[Int, Vector[Int]](catchReadError(_.toInt))
}

sealed trait PSONeighborhoodInfluence
  extends EclibParameters

case object PSONeighborhoodInfluenceBest
  extends PSONeighborhoodInfluence {
  override def toString: String = "Best solution"
}

case object PSONeighborhoodInfluenceFIPS
  extends PSONeighborhoodInfluence {
  override def toString: String = "FIPS (weighted sum with constant weights)"
}

case object PSONeighborhoodInfluencewFIPS
  extends PSONeighborhoodInfluence {
  override def toString: String = "wFIPS (weighted sum by fitness)"
}

case object PSONeighborhoodInfluencewdFIPS
  extends PSONeighborhoodInfluence {
  override def toString: String = "wdFIPS (weighted sum by Euclidean distance)"
}

object PSONeighborhoodInfluence {
  // pureconfig implicits - drop prefix from config key when matching PSONeighborhoodInfluence subclasses
  implicit val confHint = new FieldCoproductHint[PSONeighborhoodInfluence]("name") {
    override def fieldValue(name: String) = name.drop("PSONeighborhoodInfluence".length)
  }
}

case class ParticleSwarmStrategies(
                                    velocityInitialization: PSOVInitStrategies = PSOVInitZero,
                                    velocityUpdate: PSOVUpdateStrategies = PSOVUpdateStandard(),
                                    velocityLimit: Option[ParticleSwarmVelocityLimit] = None,
                                    movementLimit: Option[PSOMoveLimitStrategies] = None
                                  ) extends EclibParameters {

  override def toString: String = s"  Strategies {\n" +
    s"    Velocity initialization: $velocityInitialization\n" +
    s"    Velocity update: $velocityUpdate\n" +
    s"${ if (velocityLimit.isDefined) s"${velocityLimit.get}\n" else "" }" +
    s"${ if (movementLimit.isDefined) s"    Movement limit: ${movementLimit.get}\n" else "" }" +
    s"  }\n"

  override def check(): Boolean =
    velocityInitialization.check &&
      velocityUpdate.check &&
      (velocityLimit.isEmpty || velocityLimit.get.check) &&
      (movementLimit.isEmpty || movementLimit.get.check)
}

sealed trait PSOVInitStrategies
  extends EclibParameters

case object PSOVInitZero
    extends PSOVInitStrategies {
  override def toString: String = "Zero"
}

case class PSOVInitBounded(
                            factor: Double = 1.0
                          ) extends PSOVInitStrategies {
  override def toString: String = s"Bounded (reduction factor: $factor)"
  override def check(): Boolean =
    factor >= 0.0 && factor <= 1.0  // reduction factor of the search space limits
}

object PSOVInitStrategies {
  // pureconfig implicits - drop prefix from config key when matching PSOVInitStrategies subclasses
  implicit val confHint = new FieldCoproductHint[PSOVInitStrategies]("name") {
    override def fieldValue(name: String) = name.drop("PSOVInit".length)
  }
}

sealed trait PSOVUpdateStrategies
  extends EclibParameters

case class PSOVUpdateStandard(
                               w: Double = 1.0, // inertial coefficient. Recommended value in [0.8, 1.2]
                               c1: Double = 2.0, // cognitive coefficient. Usually in the range [0, 2], recommended value 2
                               c2: Double = 2.0, // social coefficient. Usually in the range [0, 2], recommended value 2
                               strategies: Option[PSOVUpdateStandardStrategies] = None
                             ) extends PSOVUpdateStrategies {
  override def toString: String =
    s"Standard (w: $w, c1: $c1, c2: $c2)" +
      strategies.getOrElse("")

  override def check(): Boolean =
    w >= 0.0 &&
      (c1 >= 0.0 /*&& c1 <= 2.0*/) &&
      (c2 >= 0.0 /*&& c2 <= 2.0*/) &&
      (strategies.isEmpty || strategies.get.check)
}

case class PSOVUpdateStandardStrategies(
                                         inertiaWeightAdjustment: Option[PSOWAdjustStrategies] = None
                                       ) extends EclibParameters {

  override def toString: String =
    if (inertiaWeightAdjustment.isDefined)
      s" {\n      W adjustment: ${inertiaWeightAdjustment.get}\n    }"
    else ""

  override def check(): Boolean =
    (inertiaWeightAdjustment.isEmpty || inertiaWeightAdjustment.get.check)
}

sealed trait PSOWAdjustStrategies
  extends EclibParameters

case class PSOWLinearDecreasingTime(
                                     wMin: Double
                                   ) extends PSOWAdjustStrategies {
  override def toString: String = s"Linear decreasing with time until $wMin"
}

case class PSOWLinearDecreasingGenerations(
                                            wMin: Double
                                          ) extends PSOWAdjustStrategies {
  override def toString: String = s"Linear decreasing with generations until $wMin"
}

case object PSOWRandom
  extends PSOWAdjustStrategies {
  override def toString: String = "Random"
}

case class PSOWChaoticTime(
                            wMin: Double
                          ) extends PSOWAdjustStrategies {
  override def toString: String = s"Chaotic descending with time until $wMin"
}

case class PSOWChaoticGenerations(
                                   wMin: Double
                                 ) extends PSOWAdjustStrategies {
  override def toString: String = s"Chaotic descending with generations until $wMin"
}

case object PSOWChaoticRandom
  extends PSOWAdjustStrategies {
  override def toString: String = "Chaotic random"
}

object PSOWAdjustStrategies {
  // pureconfig implicits - drop prefix from config key when matching PSOWAdjustStrategies subclasses
  implicit val confHint = new FieldCoproductHint[PSOWAdjustStrategies]("name") {
    override def fieldValue(name: String) = name.drop("PSOW".length)
  }
}

case class PSOVUpdateConstrictionFactor(
                                         c1: Double = 2.05, // cognitive coefficient. Default 2.05 (Clerc&Kennedy, 2002)
                                         c2: Double = 2.05 // social coefficient. Default 2.05 (Clerc&Kennedy, 2002)
                                       ) extends PSOVUpdateStrategies {
  override def toString: String = s"Constriction Factor (c1: $c1, c2: $c2)"
  override def check(): Boolean = c1 >= 0.0 && c2 >= 0.0
}

case class PSOVUpdateCondensedConstrictionFactor(
                           cmax: Double
                         ) extends PSOVUpdateStrategies {
  override def toString: String = s"Condensed Constriction Factor (cmax = $cmax)"
  override def check(): Boolean = cmax > 0.0
}

object PSOVUpdateStrategies {
  // pureconfig implicits - drop prefix from config key when matching PSOVUpdateStrategies subclasses
  implicit val confHint = new FieldCoproductHint[PSOVUpdateStrategies]("name") {
    override def fieldValue(name: String) = name.drop("PSOVUpdate".length)
  }
}

case class ParticleSwarmVelocityLimit(
                                       vmax: Bound,
                                       vmaxInitialization: Option[PSOVMaxInitStrategies] = None,
                                       vmaxReduce: Option[PSOVMaxReduceStrategies] = None
                                        ) extends EclibParameters {

  private lazy val vmaxInitMsg = vmaxInitialization match {
    case Some(vi) => s"      Vmax initialization: $vi\n"
    case _ => ""
  }

  private lazy val vmaxReduceMsg = vmaxReduce match {
    case Some(vr) => s"      Vmax reduce $vr\n"
    case _ => ""
  }

  override def toString: String = s"    Velocity limit {\n" +
    s"      Vmax: ${vmax.mkString("[",",","]")}\n" +
    vmaxInitMsg +
    vmaxReduceMsg +
    s"    }"

  override def check(): Boolean =
    vmax.isEmpty != vmaxInitialization.isEmpty && // only one of them can be defined
      (vmaxInitialization.isEmpty || vmaxInitialization.get.check) &&
      (vmaxReduce.isEmpty || vmaxReduce.get.check)
}

sealed trait PSOVMaxInitStrategies
  extends EclibParameters

// Vmax = k*(Xmax-Xmin)/2
case class PSOVMaxInitFactor (
                            k: Double = 1.0
                          ) extends PSOVMaxInitStrategies {
  override def toString: String = s"factor = $k"
  override def check(): Boolean =
    k > 0.0 && k <= 1.0  // reduction factor of the search space limits
}

object PSOVMaxInitStrategies {
  // pureconfig implicits - drop prefix from config key when matching PSOVMaxInitStrategies subclasses
  implicit val confHint = new FieldCoproductHint[PSOVMaxInitStrategies]("name") {
    override def fieldValue(name: String) = name.drop("PSOVMaxInit".length)
  }
}

sealed trait PSOVMaxReduceStrategies
  extends EclibParameters

// Schutte et al. Linearly Decreasing method (LDVM)
case class PSOVMaxReduceLDVM (
                             h: Generations,
                             alfa: Double,
                             beta: Double
                           ) extends PSOVMaxReduceStrategies {
  override def toString: String =
    s" {\n" +
      s"        Generations: $h\n" +
      s"        alfa: $alfa\n" +
      s"        beta: $beta\n" +
      "      }"

  override def check(): Boolean =
    h > Generations.Zero &&
      alfa > 0.0 && alfa < 1.0 &&
      beta > 0.0 && beta < 1.0
}

object PSOVMaxReduceStrategies {
  // pureconfig implicits - drop prefix from config key when matching PSOVMaxReduceStrategies subclasses
  implicit val confHint = new FieldCoproductHint[PSOVMaxReduceStrategies]("name") {
    override def fieldValue(name: String) = name.drop("PSOVMaxReduce".length)
  }
}

sealed trait PSOMoveLimitStrategies
  extends EclibParameters

case object PSOMoveLimitSearchSpace
  extends PSOMoveLimitStrategies {
  override def toString: String = "Bound to search space limits and reset velocity"
}

object PSOMoveLimitStrategies {
  // pureconfig implicits - drop prefix from config key when matching PSOMoveLimitStrategies subclasses
  implicit val confHint = new FieldCoproductHint[PSOMoveLimitStrategies]("name") {
    override def fieldValue(name: String) = name.drop("PSOMoveLimit".length)
  }
}

/** ExperimentParameters ----------------------------------------------------------------------- */

case class ExperimentParameters(
                                 name: String,
                                 description: Option[String],
                                 repetitions: Int = 1,
                                 populationSize: Int,
                                 algorithm: AlgorithmParameters,
                                 evaluationFunction: String,
                                 elementGeneration: ElementGenerationParameters,
                                 searchSpace: SearchSpaceParameters,
                                 terminationCriteria: TerminationCriteria
                               ) extends EclibParameters {

  // configuration postprocessing
  // used for calculated parameters (e.g. expand collections based on a condensed syntax)
  override def postProcess(): ExperimentParameters = this.copy(searchSpace = searchSpace.postProcess())

  override def toString: String = s"\n[[ Experiment: $name ======================================================================= ]]\n" +
    s"Description: ${description.getOrElse("")}\n" +
    s"Repetitions: $repetitions\n" +
    s"Population size: $populationSize\n" +
    algorithm +
    s"[Objective function]\n  $evaluationFunction\n" +
    elementGeneration +
    searchSpace +
    terminationCriteria

  override def check(): Boolean =
    name.nonEmpty &&
      repetitions > 0 &&
      populationSize > 0 &&
      evaluationFunction.nonEmpty &&
      elementGeneration.check &&
      algorithm.check &&
      searchSpace.check &&
      terminationCriteria.check
}

/*
case class CommonStrategies(
    initialPopulationImprovement: String = "Identity",
    finalSolutionImprovement: String = "Identity")
  extends EclibParameters {
  override def toString: String = s"Common strategies {\n" +
    s"  Initial population improvement: ${if (initialPopulationImprovement.isEmpty) "Default" else initialPopulationImprovement}\n" +
    s"  Final solution improvement: ${if (finalSolutionImprovement.isEmpty) "Default" else finalSolutionImprovement}\n" +
    s"  }\n"
}
*/

/*
//@SerialVersionUID(0L)
case class MetaheuristicParameters (
    algorithm: AlgorithmParameters,
    commonStrategies: Option[CommonStrategies] = None)
  extends EclibParameters {
  override def toString: String = algorithm + commonStrategies.getOrElse("")
  override def check = algorithm.check
}
*/

/** ExternalLibrariesParameters ---------------------------------------------------------------- */

case class ExternalLibrariesParameters(
                                        sacessHome: String,
                                        sbeclibFile: Option[String]
                                      ) extends EclibParameters {
  def sbeclibPath: String = sbeclibFile.getOrElse(s"$sacessHome/eclib/lib/libSBeclib.so")

  override def toString: String = s"[External libraries]\n" +
    s"  SACESS home: $sacessHome\n" +
    s"  ECLIB Systems Biology library file: ${sbeclibFile.getOrElse("")}\n"

  override def check(): Boolean = sacessHome.nonEmpty
}

/** ExecutionFrameworkParameters --------------------------------------------------------------- */

sealed trait ExecutionFrameworkParameters
  extends EclibParameters

object ExecutionFrameworkParameters {
  // pureconfig implicits - drop "Parameters" from config key when matching ExecutionFrameworkParameters subclasses
  implicit val confHint = new FieldCoproductHint[ExecutionFrameworkParameters]("name") {
    override def fieldValue(name: String) = name.dropRight("Parameters".length)
  }
}

/** Spark execution framework parameters
 *
 * */
case class SparkParameters(
                            jobName: String,
                            localRun: Boolean = true,
                            workers: Int = 1 // TODO: change this to pass it through a command option
                          ) extends ExecutionFrameworkParameters {
  override def toString: String = s"[Framework: Spark]\n" +
    s"  Job name: $jobName\n" +
    s"  Local run: $localRun\n" +
    s"  Workers: $workers\n"

  override def check(): Boolean = jobName.nonEmpty && workers > 0
}

/** EclibConfiguration ------------------------------------------------------------------------- */

case class EclibConfiguration(
                               executionFramework: Option[ExecutionFrameworkParameters],
                               libraries: Option[ExternalLibrariesParameters],
                               experiments: List[ExperimentParameters]
                             ) extends EclibParameters {

  override def postProcess() = this.copy(experiments = experiments.map(_.postProcess()))

  override def toString: String = "\n############################################## ECLIB ###############################################\n" +
    executionFramework.getOrElse("") +
    libraries.getOrElse("") +
    experiments.mkString("", "", "") +
    "\n#####################################################################################################\n"

  override def check(): Boolean =
    ((executionFramework, libraries) match {
      case (None, None) => true
      case (None, Some(lib)) => lib.check
      case (Some(framework), None) => framework.check
      case (Some(framework), Some(lib)) => framework.check && lib.check
    }) && experiments.forall(_.check)
}

object EclibConfiguration extends LazyLogging {
  // pureconfig implicit: format conversion from config keys to class field names
  // v2.11
  // implicit def conv[T]: ConfigFieldMapping[T] = ConfigFieldMapping.apply[T](CamelCase, SnakeCase)
  // v2.12
  implicit def hint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, SnakeCase))

  // bring other required implicits into scope
  import ParticleSwarmTopologyShape.topologyReader

  /** Loads the ECLIB configuration from a file, if provided, or search for a
   *  configuration in default locations ([[https://github.com/lightbend/config#standard-behavior]])
   *
   * @param file the location of the configuration file or None
   * @return the ECLIB configuration or None
   */
  def apply(file: Option[String]): Option[EclibConfiguration] = {
    val config = file match {  // try to load the configuration
      case Some(f) =>
        logger.info(s"Loading configuration from file: $f")
        ConfigSource.file(new File(f).toPath).load[EclibConfiguration]
      case None =>
        logger.info(s"Searching for a configuration in default locations")
        ConfigSource.default.load[EclibConfiguration]
    }
    config match {
      case Left(err) =>
        logger.error(s"$err")
        logger.error("Eclib configuration load failed. Exiting.")
        None
      case Right(c) =>
        logger.debug(s"$c")
        if (c.check) Some(c.postProcess())  // check if the configuration is valid and postprocess it
        else {
          logger.error("Eclib configuration check failed.")
          None
        }
    }
  }
}

