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
package gal.udc.gac.eclib.metaheuristics.particleswarm.topology

import com.typesafe.scalalogging.LazyLogging
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.metaheuristics.particleswarm.ParticleID
import gal.udc.gac.eclib.util.Random.sampleN
import org.apache.commons.math3.distribution.UniformIntegerDistribution
import scalax.collection.GraphPredef._
import scalax.collection.constrained.mutable.Graph


object TopologyGenerator {

  /** JGraphT generators wrapper
    *
    * See: https://jgrapht.org/javadoc/org.jgrapht.core/org/jgrapht/generate/package-summary.html
    */
  private object JGraphTGenerator {

    import org.jgrapht.generate._
    import org.jgrapht.graph.DefaultEdge
    import TopologyWrappers.JGraphTWrapper._
    import TopologyWrappers.JGraphTWrapper.Implicits._
    import java.util.Calendar

    /** the JGraphTGenerator interface parametrized with PSO Topology types */
    private type JGraphTGenerator[T] = GraphGenerator[ParticleID, DefaultEdge, T]

    /** creates a new topology
      *
      * @param generator the JGraphT generator from which to create the topology
      * @return the topology
      */
    // TODO: return None when JGraphTGenerator throws an exception
    private def apply[T](generator: JGraphTGenerator[T]): Option[Topology] = {
      val graph = JGraphTSimpleDirectedGraph()
      generator.generateGraph(graph)
      Some(graph)
    }

    /** creates a new topology
      *
      * @param sz the number of vertices (size of the swarm)
      * @param param the parameters of the topology to create
      * @return the topology
      */
    def apply(sz: Int, param: ParticleSwarmTopologyShape): Option[Topology] = param match {
//      case PSOTopologyComplete => apply(new CompleteGraphGenerator(sz))
      case PSOTopologyDRing(_) => apply(new RingGraphGenerator(sz))  // DRing1
      case PSOTopologyGrid(rows, cols) =>
        require (rows > 1 && cols > 1, "The number of rows and columns must be greater than 1 in a Grid topology")
        require (rows*cols == sz, "rows*columns must be equal to the population size in a Grid topology")
        apply(new GridGraphGenerator(rows, cols))
      case PSOTopologyLinear => apply(new LinearGraphGenerator(sz))
      case PSOTopologyStar => apply(new StarGraphGenerator(sz))
      case PSOTopologyWheel => apply(new WheelGraphGenerator(sz))
      case PSOTopologyGnmRandom(edges) =>
        require(edges >= sz-1, "The number of edges must be at least the population size-1 in a GnmRandom topology")
        require(edges <= sz*(sz-1), "The number of edges must be lower than population size*(population size - 1) in a GnmRandom topology")
        apply(new GnmRandomGraphGenerator(sz, edges)) match {
          case None => None
          case Some(t) =>
            assert(t.nodes.size == sz, "The GnmRandom generator has generated a non connected topology. Maybe the number of edges is too low.")
            Some(t)
        }
      case PSOTopologyGnpRandomBipartite(n1, n2, p) =>
        require(n1+n2 == sz, "n1+n2 must be equal to the population size in a GnpRandomBipartite topology")
        apply(new GnpRandomBipartiteGraphGenerator(n1, n2, p)) match {
          case None => None
          case Some(t) =>
            assert(t.nodes.size == sz, "The GnpRandom generator has generated a non connected topology. Maybe the value of p is too low.")
            Some(t)
        }
      case PSOTopologyHyperCube(dim) =>
        require(math.pow(2, dim) == sz, "2^dim must be equal to the population size in a HyperCube topology")
        apply(new HyperCubeGraphGenerator(dim))
      case PSOTopologyScaleFree(alfa, gamma, deltaIn, deltaOut) =>
        apply(new DirectedScaleFreeGraphGenerator(alfa, gamma, deltaIn, deltaOut, -1, sz, Calendar.getInstance().getTimeInMillis, false, false))
      case PSOTopologyScaleFreeNetwork =>
        apply(new ScaleFreeGraphGenerator(sz))
      case PSOTopologyGeneralizedPetersen(k) =>
        require(sz%2 == 0, "The population size must be even in a GeneralizedPetersen topology")
        require(k>0 && k<=math.floor((sz/2-1)/2.0), "k must be in the range [1, floor((population size/2 - 1)/2.0)] in a GeneralizedPetersen topology")
        apply(new GeneralizedPetersenGraphGenerator(sz/2, k))
      case PSOTopologyKleinbergSmallWorld(p, q, r) =>
        require(math.pow(math.sqrt(sz), 2) == sz, "The population size has to be a perfect square in a KleinbergSmallWorld topology")
        apply(new KleinbergSmallWorldGraphGenerator(math.sqrt(sz).toInt, p, q, r))
      case PSOTopologyPlantedPartition(l, p, q) =>
        require(sz%l == 0, "The population size must be a multiple of l in a PlantedPartition topology")
        apply(new PlantedPartitionGraphGenerator(l, sz/l, p, q, false))
      case PSOTopologyWindmill(m) =>
        val n = (sz+m-1)/m  // number of nodes in each complete group
        // the population size has to be equal to the number of groups (complete graphs) * group size minus 1 plus the common vertex
        require(m*(n-1)+1 == sz, "The population size must be equal to m*(complete graph size-1)+1 in a Windmill topology")
        apply(new WindmillGraphsGenerator(WindmillGraphsGenerator.Mode.WINDMILL, m, n))
      case PSOTopologyDutchWindmill(m) =>
        val n = (sz+m-1)/m  // number of nodes in each cycle group
        // the population size has to be equal to the number of groups (cycle graphs) * group size minus 1 plus the common vertex
        require(m*(n-1)+1 == sz, "The population size must be equal to m*(cycle graph size-1)+1 in a DutchWindmill topology")
        apply(new WindmillGraphsGenerator(WindmillGraphsGenerator.Mode.DUTCHWINDMILL, m, n))
      case _ => None
    }
  } // JGraphTGenerator

  /** Graph4Scala random generator wrapper */
  private object RandomGenerator extends LazyLogging {

    import scalax.collection.generator.{NodeDegreeRange, RandomGraph}

     case class TopologyMetrics(order: Int, nodeDegrees: NodeDegreeRange)
      extends RandomGraph.IntFactory {
      //override def connected = false
      private var current, next = 0

      logger.debug(s"TopologyMetrics: $order, $nodeDegrees")

      override def nodeGen: Int = {
        current = next
        next = (next + 1) % order
        current
      }
    }

    def apply(sz: Int, dMin: Int, dMax:Int): Option[Topology] = {

      require(dMin >= 2 && dMin <= dMax && dMax < sz, "In a Random topology it must be 2 <= d_min <= d_max < population size")

      val topo: Topology = Graph.empty
      val graph = RandomGraph.diGraph[ParticleID, Graph](Graph, TopologyMetrics(sz,  NodeDegreeRange(dMin, dMax))).draw
      graph.edges.toOuter foreach (edge => topo += edge)
      Some(topo)
    }
  } // GRandom

  private object RegularRandomGenerator extends LazyLogging {
    def apply(sz: Int, k: Int): Option[Topology] = {

      require(k > 1 && k < sz, "The degree has to be in the range (1, population size) in a RegularRandom topology")

      val random = new UniformIntegerDistribution(0, sz-1)
      // for each source node generate k+1 distinct random targets,
      // filter the source to avoid self-loops and create k edges
      val edges = for (
        source <- (0 to sz - 1);
        target <- sampleN(k+1, random).filter(_ != source).take(k)
      ) yield (source ~> target)
      Some(Topology() ++ edges)
    }
  }

  // This generator replaces the JGraphT complete generator because is less efficient
  private object CompleteGenerator extends LazyLogging {
    def apply(sz: Int): Option[Topology] = {
      val edges = for (
        source <- (0 to sz - 1);
        target <- (0 to sz - 1);
        if (source != target)) yield (source ~> target)
      Some(Topology() ++ edges)
    }
  }

  private object DRingGenerator {

    def apply(sz: Int, param: PSOTopologyDRing): Option[Topology] = {

      require (param.k > 0 && param.k < sz, "The degree has to be in the range (0, population size) in a DRing topology")

      // complete a given DRing(1) with edges to the k-1 following neighbors
      def edgesToKNeighbors =
        (0 until sz).flatMap(source =>
          (source+2 to source + param.k).map(target =>
            source ~> (target%sz)))

      JGraphTGenerator(sz, param) match {  // create a DRing(1)
        case Some(ring1) =>
          if (param.k == 1) Some(ring1)
          else Some(ring1 ++ edgesToKNeighbors) // add missed edges
        case None => None
      }
    }

  } // DRingGenerator

  private object RingGenerator {

    def apply(sz: Int, k: Int): Option[Topology] = {

      require (k > 0 && k <= sz/2, "The degree has to be in the range (0, population size/2] in a Ring topology")

      /** Create a Ring from a Directed Ring by adding
        * the reverse of existing edges in the given Directed Ring
        */
      def directedRingToRing(dring: Topology): Topology =
        dring ++ (dring.edges.toOuter map (edge => edge._2 ~> edge._1))

      DRingGenerator(sz, PSOTopologyDRing(k)) match { // create a DRing(k)
        case Some(dring) => Some(directedRingToRing(dring)) // add missed edges
        case None => None
      }
    }
  } // RingGenerator

  private object SquareGenerator {

    def apply(sz: Int, rows: Int, cols: Int): Option[Topology] = {

      require (rows > 1 && cols > 1, "The number of rows and columns must be greater than 1 in a Square topology")
      require (rows*cols == sz, "rows*columns must be equal to the population size in a Square topology")

      def closingEdges = {

        // edges between nodes in first and last rows
        val verticalEdges = 0 until cols flatMap (source => {
            val target = (rows-1)*cols + source
            List(source ~> target, target ~> source)
          })

        // edges between nodes in first and last columns
        val horizontalEdges =  0 until rows flatMap (row => {
            val source = row * cols
            val target = source + cols - 1
            List(source ~> target, target ~> source)
          })

        verticalEdges ++ horizontalEdges
      }

      JGraphTGenerator(sz, PSOTopologyGrid(rows, cols)) match {  // create a Grid
        case Some(grid) =>
          Some(grid ++ closingEdges) // add missed edges at the borders
        case None => None
      }
    }
  }

  // regular topology with small-world shortcut as defined in [Liu et al. 2016]
  private object RegularGenerator {

    def apply(sz: Int, k: Int): Option[Topology] = {

      require (k > 1 && k < sz, "The degree has to be in the range (1, population size) in a Regular topology")
      require ((sz*k)%2 == 0, "population size*k has to be even in a Regular topology")

      /** Add small-world shortcuts */
      def addSmallWorldShortcuts(ring: Topology): Topology = {
        ring ++ (ring.nodes.toOuter flatMap (source => {
          val target = (source+sz/2)%sz
          List(source ~> target, target ~> source)
        }))
      }

      RingGenerator(sz, k/2) match { // create a Ring(k/2)
        case Some(ring) =>
          if (k%2 == 0) Some(ring)
          else Some(addSmallWorldShortcuts(ring)) // add small-world shortcuts
        case None => None
      }
    }
  } // RegularGenerator

  /** A triangular mesh graph */
  private object PyramidGenerator {

    import Delaunay.{Triangulation, Vector2}

    private val size = 100.0

    def apply(sz: Int): Option[Topology] = {

      // particles are distributed randomly in a size x size square
      val points = (0 until sz) map (i =>
        Vector2(i*size/sz, math.random()*size))

      // call Delaunay triangulation to get the triangular mesh
      val edges = Triangulation(points.toList) flatMap { triangle =>
        List(
          triangle._1 ~> triangle._2,
          triangle._2 ~> triangle._1,
          triangle._1 ~> triangle._3,
          triangle._3 ~> triangle._1,
          triangle._2 ~> triangle._3,
          triangle._3 ~> triangle._2)
      }

      Some(Topology() ++ edges)
    }
  } // PyramidGenerator

  /** K complete graphs connected by disjoint vertices */
  private object KClustersGenerator {

    def apply(sz: Int, k: Int): Option[Topology] = {

      require(sz%k == 0, "The population size must be a multiple of k in a KCluster topology")
      // the number of clusters has to be lower than the number of vertex in each cluster
      require(sz/k >= k-1, "The cluster size (population size/k) must be greater than k-2 in a KCluster topology")

      def connectionEdges =
        (0 until k-1) flatMap (i =>
          (i until k-1) flatMap (j =>
            List(
              (j+i*sz/k) ~> (i+(j+1)*sz/k),
              (i+(j+1)*sz/k) ~> (j+i*sz/k)
            )))

      // create k complete disconnected graphs
      TopologyGenerator(sz, PSOTopologyPlantedPartition(k, 1, 0)) match {
        case None => None
        case Some(graph) => Some(graph ++ connectionEdges)
      }
    }
  } // KClustersGenerator

  private object CustomTopologyGenerator {

    def apply(sz: Int, c: PSOTopologyCustom): Option[Topology] = {

      require(c.neighborhoods.keys.size == sz, "The number of vertices must be equal to the population size in a Custom topology")
      require(!c.neighborhoods.keys.exists(id => id < 0 || id >= sz), "The vertices' ids must be values in the range [0, population size) in a Custom topology")
      require(!c.neighborhoods.values.exists(_.exists(id => id < 0 || id >= sz)), "The vertices' ids must be values in the range [0, population size-1) in a Custom topology")
      require(!c.neighborhoods.exists {case (vertex, neighbors) => neighbors contains vertex}, "A topology can not have self-loops")
      require(!c.neighborhoods.values.exists(neighbors => neighbors.size != neighbors.toSet.size) , "A topology can not have duplicated edges")

      // create an empty topology and add edges
      val topo: Topology = Graph.empty
      c.neighborhoods map { case(vertex, neighbors) =>
        topo ++= neighbors.map(vertex ~> _)
      }
      assert(topo.nodes.size == sz, "A custom topology must be a complete graph")
      Some(topo)
    }
  } // CustomTopologyGenerator

  private object TopologyFromDotFileGenerator {

    import gal.udc.gac.eclib.metaheuristics.particleswarm.topology.TopologyIO.FileIO


    def apply(sz: Int, file: String): Option[Topology] = {

      require(file.nonEmpty, "dot file path can not be empty")

      // read the topology from DOT file
      Topology.fromDot(FileIO.read(file).getOrElse("")) match {
        case None => None
        case Some(topo) =>
          require(topo.nodes.size == sz, "The number of vertices must be equal to the population size in a Custom topology")
          require(!topo.nodes.exists(id => id < 0 || id >= sz), "The vertices' ids must be values in the range [0, population size) in a Custom topology")
          Some(topo)
      }

    }
  } // TopologyFromDotFileGenerator

      /** General Topology generator
    *
    * It wraps different other generators:
    *   JGraphT generators [[https://jgrapht.org/javadoc/org.jgrapht.core/org/jgrapht/generate/package-summary.html]]
    *   Graph4Scala RandomGenerator
    * and implements Ring, DRing, VonNewman, Regular
    */
  def apply(sz: Int, param: ParticleSwarmTopologyShape): Option[Topology] = param match {
    case PSOTopologyGRandom(dMin, dMax) => RandomGenerator(sz, dMin, dMax)
    case PSOTopologyRegularRandom(k) => RegularRandomGenerator(sz, k)
    case PSOTopologyComplete => CompleteGenerator(sz)
    case dr:PSOTopologyDRing => DRingGenerator(sz, dr)
    case PSOTopologyRing(k) => RingGenerator(sz, k)
    case PSOTopologySquare(rows, cols) => SquareGenerator(sz, rows, cols)
    case PSOTopologyRegular(k) => RegularGenerator(sz, k)
    case PSOTopologyPyramid => PyramidGenerator(sz)
    case PSOTopologyKClusters(k) => KClustersGenerator(sz, k)
    case c:PSOTopologyCustom => CustomTopologyGenerator(sz, c)
    case PSOTopologyCustomFromDotFile(file) => TopologyFromDotFileGenerator(sz, file)
    case _ => JGraphTGenerator(sz, param)
  }
}