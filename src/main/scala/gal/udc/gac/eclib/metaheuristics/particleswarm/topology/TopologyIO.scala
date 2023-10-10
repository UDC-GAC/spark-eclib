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

object TopologyIO {

  // TODO: support other Importers/Exporters (i.e. JGraphT importers/exportes, Graph4Scala JSON)

  implicit class TopologyConvertOps(t: Topology) {
    def asDot(id: String): String = DotExporter.exportTopology(id, t)
  }

  trait TopologyImportOps {
    def fromDot(dot: String): Option[Topology] = DotImporter.importTopology(dot)
  }

  /** Import dot files using the JGraphT DOTImporter
    *
    * Only directed graphs with Int IDs and without labels,
    * weights or self-loops are supported
    */
  object DotImporter {

    import java.io.StringReader
    import org.jgrapht.nio.dot.{DOTImporter => JGraphTDOTImporter}
    import org.jgrapht.graph.DefaultEdge
    import TopologyWrappers.JGraphTWrapper.JGraphTSimpleDirectedGraph
    import TopologyWrappers.JGraphTWrapper.Implicits._

    /** the JGraphTGenerator interface parametrized with PSO Topology types */
    private type JDOTImporter = JGraphTDOTImporter[ParticleID, DefaultEdge]
    private object JDOTImporter {
      def apply(): JDOTImporter = {
        val importer = new JGraphTDOTImporter[ParticleID, DefaultEdge]
        importer.setVertexFactory(_.toInt)  // vertices are created using the Int ID
        importer
      }
    }

    def importTopology(dot: String): Option[Topology] = {
      val graph = JGraphTSimpleDirectedGraph()
      JDOTImporter().importGraph(graph, new StringReader(dot))
      Some(graph)
    }
  }

  object DotExporter {

    import scalax.collection.io.dot.{DotEdgeStmt, DotRootGraph, graph2DotExport}
    import scalax.collection.io.dot.implicits._

    def exportTopology(id: String, t: Topology): String = {
      val root = DotRootGraph(directed = true, Some(id))
      t.toDot(root, edge => Some(root, DotEdgeStmt(edge._1.toString, edge._2.toString)))
    }
  }

  object FileIO {

    import java.io.Closeable
    import scala.io.Source
    import java.io.{File, FileNotFoundException, PrintWriter}
    import scala.util.control.Exception.catching

    /** implementation of the Loan pattern
      *
      * it closes a resource (file) in case of failure
      */
    private object LoanPattern {
      private[FileIO] def using[A <: Closeable, B](resource: A)(f: A => B): B =
        try {
          f(resource)
        } finally {
          resource.close()
        }
    }

    import LoanPattern._

    def read(file: String): Option[String] = {
      catching(
          classOf[NullPointerException],
          classOf[FileNotFoundException]) opt {
        using(Source.fromFile(file)) { _.getLines.mkString }
      } match {
        case Some(content) => Some(content)
        case None => println(s"Can not read $file"); None
      }
    }

    def write(file: String, source: String): Unit = {
      catching(
        classOf[NullPointerException],
        classOf[SecurityException],
        classOf[FileNotFoundException]) opt {
        new PrintWriter(new File(file))
      } match {
        case Some(writer) => writer.write(source);  writer.close()
        case None => println(s"Can not open $file")
      }
    }
  }
}