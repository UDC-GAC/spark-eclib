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

import scalax.collection.GraphPredef._
import scalax.collection.constrained._

//class NoSelfLoop[N, E[+X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G) // v1.13.2 (Scala 2.13)
class NoSelfLoop[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G) // v1.13.0 (Scala 2.12)
  extends Constraint[N, E, G](self) {

  import PreCheckFollowUp._

  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]): PreCheckResult = // v1.13.0 (Scala 2.12)
  //override def preCreate(nodes: Iterable[N], edges: Iterable[E[N]]): PreCheckResult = // v1.13.2 (Scala 2.13)
    PreCheckResult.complete(edges forall (!preAdd(_).abort)) // check edges only

  override def preAdd(node: N): PreCheckResult = PreCheckResult(Complete)
  override def preAdd(edge: E[N]): PreCheckResult = PreCheckResult.complete(edge.nonLooping) // abort if self-loop
  override def preSubtract(node: self.NodeT, forced: Boolean): PreCheckResult = PreCheckResult(Complete)
  override def preSubtract(edge: self.EdgeT, forced: Boolean): PreCheckResult = PreCheckResult(Complete)
}

object NoSelfLoop extends ConstraintCompanion[NoSelfLoop] {
//  def apply[N, E[+X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new NoSelfLoop[N, E, G](self) // v1.13.2 (Scala 2.13)
  def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new NoSelfLoop[N, E, G](self) // v1.13.0 (Scala 2.12)
}
