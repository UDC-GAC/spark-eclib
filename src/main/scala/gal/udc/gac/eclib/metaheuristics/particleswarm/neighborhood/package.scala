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

import scala.reflect.ClassTag
import scala.language.postfixOps

import com.typesafe.scalalogging.LazyLogging

import gal.udc.gac.eclib.metaheuristics.particleswarm.topology.Topology

// neighborhood-related definitions
// they are used to calculate the contribution of a particle's neighborhood to the social component of its velocity
package object neighborhood {

  type ParticleContribution[T] = T
  type ParticleContributionFunction[T] = Particle => ParticleContribution[T]

  type NeighborhoodContribution[T] = Map[ParticleID, ParticleContribution[T]]
  type NeighborhoodContributionFunction[T, S] = (ParticleContribution[T], NeighborhoodContribution[T]) => S

  type NeighborhoodInfluence[S] = Map[ParticleID, S]
  type NeighborhoodInfluenceFunction[S] = ParticleID => S

  trait NeighborhoodOps extends LazyLogging {

    // method to collect the contribution of each particle to the neighborhood
    def contribution[T](pf: ParticleContributionFunction[T])(
      implicit ct: ClassTag[T]): NeighborhoodContribution[T]

    // a generic neighborhood influence function builder
    // it is not optimised for special cases like the global (full-connected) topology
    def neighborhoodInfluence[T, S](self: Boolean = true)(t: Topology)(collect: ParticleContributionFunction[T])(reduce: NeighborhoodContributionFunction[T, S])(
      implicit ct: ClassTag[T]): NeighborhoodInfluenceFunction[S] = {

      // collect the contribution of each particle
      // TODO? do not collect particles without outgoing edges in the topology
      val contrib: NeighborhoodContribution[T] = contribution(collect)

      logger.trace(s"Topology: $t")
      logger.trace(s"Contribution: $contrib")

      // compute the contribution of each particle's neighborhood as a map
      val influence: NeighborhoodInfluence[S] = t.nodes map (p => {
        val neighborhood = {
          // if inNeighbors is empty then the particle is the neighborhood
          // (the social component of the velocity will be zero after update)
          if (self || p.inNeighbors.isEmpty) p.inNeighbors + p
          else p.inNeighbors
        } map {_.value}
        logger.trace(s"Neighborhood of ${p.toOuter}: $neighborhood")
        p.toOuter -> reduce(contrib(p.toOuter), contrib.filter(neighborhood contains _._1))
      }) toMap

      logger.trace(s"Neighborhood influence: $influence")
      // return the neighborhood influence function that maps each particle's ID
      // to its neighborhood's contribution
      id => influence(id)
    }
  } // NeighborhoodOps
}
