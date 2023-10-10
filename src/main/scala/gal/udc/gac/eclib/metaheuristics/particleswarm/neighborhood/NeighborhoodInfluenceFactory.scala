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
package gal.udc.gac.eclib.metaheuristics.particleswarm.neighborhood

import com.typesafe.scalalogging.LazyLogging
import gal.udc.gac.eclib._
import gal.udc.gac.eclib.modules._
import gal.udc.gac.eclib.searchspace._
import gal.udc.gac.eclib.population.Individual
import gal.udc.gac.eclib.metaheuristics.particleswarm._
import gal.udc.gac.eclib.metaheuristics.particleswarm.topology.{SerializableTopology, Topology}
import gal.udc.gac.eclib.util.Random.reals

object NeighborhoodInfluenceFactory {

  object ParticleContribution {

    /** The best particle's solution (position + fitness) found so far */
    object BestSolution {
      def apply(): ParticleContributionFunction[Individual] = particle =>
        particle.best
    }

    /** The position of the best particle's solution found so far */
    object BestPosition {
      def apply(): ParticleContributionFunction[Position] = particle =>
        particle.best.element
    }

  } // ParticleContribution

  object NeighborhoodInfluence extends LazyLogging {

    object BestNeighbor {
      def apply(): NeighborhoodContributionFunction[Individual, Position] = (_, contribs) => {
        require(contribs.nonEmpty, "A neighborhood's contribution can not be empty")
        contribs.values.min.element
      }
    }

    object FullInformed {

      /** Common weighted sum used by the other FIPS neighborhood influence implementations
        *
        * @note pk and wk must have the same size and it must be greater than zero
        */
      private def apply(cmax: Double, wk: Iterable[Double], pk: Iterable[Position]): Element = {
        val (n, dim) = (pk.size, pk.head.size)
        assert(dim > 0, "The size of a contribution must be greater than zero")
        val ck = cmax / n
        // generate n random vectors of dim uniformly distributed random numbers in [0,ck)
        val r = (1 to n) map (_ => reals.sample(dim).map(_ * ck).toVector)
        val wkr = wk.zip(r) map (wr => wr._2 map (_ * wr._1))
        val wpk = pk.zip(wkr) map (pp => pp._1.combine(pp._2)((a, b) => a * b))
        val numerator = wpk.foldRight(Element.Zero(dim))((wpi, acc) =>
          wpi.combine(acc)((a, b) => a + b))
        val denominator = wkr.foldRight(Element.Zero(dim))((wir, acc) =>
          wir.combine(acc)((a, b) => a + b))
        val pm = numerator.combine(denominator)((n, d) => n / d)
        logger.whenTraceEnabled({
          logger.trace("Neighborhood Influence {")
          logger.trace(s"  N:$n, Ck:$ck, D:$dim")
          logger.trace(s"  Random: ${r.mkString(", ")}")
          logger.trace(s"  Wk: ${wk.mkString(", ")}")
          logger.trace(s"  Pk: ${pk.mkString(", ")}")
          logger.trace(s"  Wk*Rk*Pk: ${wpk.mkString(", ")}")
          logger.trace(s"  Numerator: $numerator")
          logger.trace(s"  Denominator: $denominator")
          logger.trace(s"  Pm: ${pm.mkString(", ")}")
          logger.trace("}")
        })
        pm
      }

      /** FIPS (weighted sum with constant weights) */
      object Constant {

        val w = 1.0 // which constant value to use?

        def apply(cmax: Double): NeighborhoodContributionFunction[Position, Position] = (_, contribs) => {
          require(contribs.nonEmpty, "A neighborhood's contribution can not be empty")
          val (pk, wk) = (contribs.values, Vector.fill(contribs.size)(w))
          FullInformed(cmax, wk, pk)
        }
      }

      /** wFIPS (weighted sum by fitness) */
      object Fitness {
        def apply(cmax: Double): NeighborhoodContributionFunction[Individual, Position] = (_, contribs) => {
          require(contribs.nonEmpty, "A neighborhood's contribution can not be empty")
          val (pk, wk) = contribs.values.map(i => (i.element, i.fitness)).unzip
          FullInformed(cmax, wk, pk)
        }
      }

      /** wdFIPS (weighted sum by Euclidean distance) */
      // TODO: support configurable distance functions
      object Distance {
        def apply(cmax: Double): NeighborhoodContributionFunction[Position, Position] = (particle, contribs) => {
          require(contribs.nonEmpty, "A neighborhood's contribution can not be empty")
          val (pk, wk) = (contribs.values, contribs.values map (DistanceFunction.euclidean(particle, _)))
          FullInformed(cmax, wk, pk)
        }
      }

    } // FullInformed

  } // NeighborhoodInfluence

  object Strategies {

    object OptimizedForGlobalTopology {

      object BestNeighbor extends LazyLogging {

        def apply[T](best: T): NeighborhoodInfluenceFunction[T] = _ => best

        def apply(swarm: Swarm)(
          implicit properties: PropertiesStore): NeighborhoodInfluenceFunction[Position] = {
          val p: Particle = properties(GlobalBest).as[Particle] // particle with global best
          // collect the best solutions
          val sols = swarm.contribution(ParticleContribution.BestSolution())
          assert(sols.size >= 2, "Swarm size must be >= 2")
          // get the second global best solution
          val gbest2 = (sols - p.id).values.min
          logger.trace(s"BestNeighbor => Best: ${p.best}")
          logger.trace(s"BestNeighbor => Second: $gbest2")
          // return the function that do not take self into account in GBest
          if (p.best.fitness != gbest2.fitness)
            id => if (id == p.id) gbest2.element else p.best.element
          else BestNeighbor(p.best.element) // if they have the same best fitness => is the same case as self=true
        }

      } // BestNeighbor

      def apply(swarm: Swarm, topology: ParticleSwarmTopology, cmax: Double)(
        implicit properties: PropertiesStore): NeighborhoodInfluenceFunction[Position] = topology.neighborhoodInfluence match {
        case PSONeighborhoodInfluenceBest =>
          if (topology.self) BestNeighbor(properties(GlobalBest).as[Particle].best.element)
          else BestNeighbor(swarm)
        case _ =>
          // force assert to fail => returning an Option is not possible because of the way [[StandardPSO]] is implemented
          assert(assertion = false, "Only the Best neighbor influence is supported by the Global topology. For FIPS influence use the Complete topology instead.")
          Nil
      }

    } // OptimizedForGlobalTopology

    object Default {
      // return the general neighborhood influence function defined in [[NeighborhoodOps]]
      // with custom particle and neighborhood contribution functions
      def apply(swarm: Swarm, topology: ParticleSwarmTopology, cmax: Double)(
        implicit properties: PropertiesStore): NeighborhoodInfluenceFunction[Position] = {
        val (self, topo): (Boolean, Topology) = (topology.self, properties(SwarmTopology).as[SerializableTopology])
        topology.neighborhoodInfluence match {
          case PSONeighborhoodInfluenceBest => // Best
            swarm.neighborhoodInfluence(self)(topo)(
              ParticleContribution.BestSolution())(
              NeighborhoodInfluence.BestNeighbor())
          case PSONeighborhoodInfluenceFIPS => // Full Informed
            swarm.neighborhoodInfluence(self)(topo)(
              ParticleContribution.BestPosition())(
              NeighborhoodInfluence.FullInformed.Constant(cmax))
          case PSONeighborhoodInfluencewFIPS => // Full Informed weighted by Fitness
            swarm.neighborhoodInfluence(self)(topo)(
              ParticleContribution.BestSolution())(
              NeighborhoodInfluence.FullInformed.Fitness(cmax))
          case PSONeighborhoodInfluencewdFIPS => // Full Informed weighted by Distance
            swarm.neighborhoodInfluence(self)(topo)(
              ParticleContribution.BestPosition())(
              NeighborhoodInfluence.FullInformed.Distance(cmax))
        }
      }
    } // Default

  } // Strategies

  def apply(swarm: Swarm, topology: ParticleSwarmTopology, cmax: Double)(
    implicit properties: PropertiesStore): NeighborhoodInfluenceFunction[Position] = topology.shape match {
    case PSOTopologyGlobal => Strategies.OptimizedForGlobalTopology(swarm, topology, cmax) // optimised implementation for the global topology
    case _ => Strategies.Default(swarm, topology, cmax) // default implementation
  }
}
