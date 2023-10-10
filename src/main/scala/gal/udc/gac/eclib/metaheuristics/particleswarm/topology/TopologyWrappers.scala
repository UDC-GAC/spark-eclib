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

import gal.udc.gac.eclib.metaheuristics.particleswarm.ParticleID

import scala.collection.JavaConverters._
import scalax.collection.GraphPredef._
import scalax.collection.constrained.mutable.Graph

object TopologyWrappers {

  object JGraphTWrapper {

    import org.jgrapht.graph.{DefaultEdge, SimpleGraph, SimpleDirectedGraph}
    import org.jgrapht.util.SupplierUtil

    // simple directed graph
    type JGraphTSimpleDirectedGraph = SimpleDirectedGraph[ParticleID, DefaultEdge]
    object JGraphTSimpleDirectedGraph {
      /** A ParticleID supplier to generate vertex (particles) IDs */
      private object ParticleIDSupplier {
        import java.util.function.Supplier
        private type ParticleIDSupplier = Supplier[ParticleID]
        def apply(): ParticleIDSupplier = SupplierUtil.createIntegerSupplier().asInstanceOf[ParticleIDSupplier]
        //implicit def toParticleIDSupplier(s: Supplier[Integer]): ParticleIDSupplier = s.asInstanceOf[ParticleIDSupplier]
      }

      def apply(): JGraphTSimpleDirectedGraph =
        new SimpleDirectedGraph(ParticleIDSupplier(), SupplierUtil.createDefaultEdgeSupplier(), false)
    }

    object Implicits {

      /** implicit conversion from JGraphT SimpleDirectedGraph to Topology
        *
        * @param g the graph to convert
        * @return the topology that results from converting the graph
        */
      implicit def toTopology(g: JGraphTSimpleDirectedGraph): Topology = {
        // create an empty topology
        val topo: Topology = Graph.empty
        // add edges
        g.edgeSet().asScala map (edge =>
          topo += g.getEdgeSource(edge) ~> g.getEdgeTarget(edge))
        topo
      }
    }

  }
}
