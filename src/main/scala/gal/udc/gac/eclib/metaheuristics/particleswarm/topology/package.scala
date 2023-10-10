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
package gal.udc.gac.eclib.metaheuristics.particleswarm

import gal.udc.gac.eclib._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.constrained.config.ConstrainedConfig
import scalax.collection.constrained.mutable.{CompanionAlias, Graph}

import java.io.{IOException, ObjectInputStream, ObjectOutputStream, Serializable}

package object topology {

  import TopologyIO.TopologyImportOps

  /**
    *  Topology definitions
    *
    *  A DAG is used to represent a topology (McNabb et al, 2009. An Exploration of
    *  Topologies and Communication in Large Particle Swarms).
    *
    *  It has been implemented using a a Directed Graph without self-loops
    */
  implicit val topoConfig: ConstrainedConfig = NoSelfLoop withStringPrefix "Topology"

  type Topology = Graph[ParticleID, DiEdge]

  object Topology
    extends CompanionAlias[DiEdge](topoConfig.constraintCompanion)
      with TopologyImportOps {

    object Default {
      def apply(): Topology = Graph.empty // default topology = empty graph
    }

    object Global {
      // TODO: consider using an Either to implement Global
      def apply(): Topology = Default() // an empty graph is used to represent the Global topology
    }

/*    object Custom {
      def apply(p: PSOTopologyCustom): Option[Topology] =
        mutable.Map.empty ++= p.neighborhoods // convert from inmutable to mutable Map
    }*/

    def apply(): Topology = Default()

    // create a topology from a given topology configuration
    // if index > 0, IDs are incremented by index
    def apply(sz: Int, p: ParticleSwarmTopology, index: Int = 0): Option[Topology] = apply(sz, p.shape) match {
      case Some(t) => if (index > 0) Some(t.incrParticleIDBy(index)) else Some(t)
      case None => None
    }

    def apply(sz: Int, shape: ParticleSwarmTopologyShape): Option[Topology] = shape match {
      case PSOTopologyGlobal => Some(Global())
//      case t: PSOTopologyCustom => Custom(t)
      case _ => TopologyGenerator.apply(sz, shape)
    }
  }

  implicit class TopologyOps(t: Topology) {
    def isGlobal: Boolean = t.isEmpty

    def incrParticleIDBy(index: Int): Topology =
      Topology() ++ t.edges.toOuter.map(edge => (edge._1 + index) ~> (edge._2 + index))

//    def mkString: String =
//      s"(${t.map(v => s"${v._1}->${v._2.mkString("[", ",", "]")}").toVector.sorted.reduce((a, b) => s"$a, $b")})"
  }

  /**  Workaround for adding serialization support to Topology
    *
    *  This is needed to serialize topologies to workers in Spark
    *  For some reason "off-the-shelf" serialization of Graph for Scala does not work
    *  (maybe because we are not using the most recent versions)
    *
    *  This implementation assumes that all nodes are connected
    */

  // wrapper to serialize/deserialize topologies
  @SerialVersionUID(0L)
  case class SerializableTopology(var topology: Topology = Topology()) extends Serializable {

    @throws(classOf[IOException])
    private def writeObject(out: ObjectOutputStream): Unit =
      out.writeObject(topology.edges.toOuter) // serialize outer edges

    @throws(classOf[ClassNotFoundException])
    @throws(classOf[IOException])
    private def readObject(in: ObjectInputStream): Unit  = {
      // build the graph from the deserialized edges
      topology = Graph.from(edges = in.readObject.asInstanceOf[Iterable[DiEdge[ParticleID]]])
    }
  }

  // implicit conversions between Topology and SerializableTopology
  implicit def toTopology(s: SerializableTopology): Topology = s.topology
  implicit def toSerializableTopology(t: Topology): SerializableTopology = SerializableTopology(t)
}
