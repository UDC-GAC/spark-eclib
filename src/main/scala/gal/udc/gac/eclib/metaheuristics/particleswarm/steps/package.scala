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

import gal.udc.gac.eclib.{PSOIslandsConfiguration, PSOIslandParameters}

package object steps {

  object FromIslandConfigurationsCreate {

    private[FromIslandConfigurationsCreate] object IslandToConfigurationMapping {

      /**
        * Functions to map between island/partition IDs and island configuration options
        * They are used to support different ways of specifying the islands configuration (e.g. by amount, by percentage, ...)
        */

      type IslandToConfigurationMapping = Int => Int
      type IslandToConfigurationMappingFactory = (Int, Vector[Int]) => IslandToConfigurationMapping

      def asAmount(islands: Int, amounts: Vector[Int]): IslandToConfigurationMapping = {
        require(islands <= amounts.sum, "The number of partitions must be less than or equal to the total amount of islands configured")
        val sums = (1 to amounts.size).map(amounts.take(_).sum) // cumulative sum of the amount values
        i => sums.indexWhere(i < _) // return the mapping function
      }

      def asPercentage(islands: Int, amounts: Vector[Int]): IslandToConfigurationMapping = {
        val total = amounts.sum.toDouble
        val percentages = amounts.map(amount => math.round(amount / total * 100)) // percentages (they are rounded)
        val sums = (1 to percentages.size).map(percentages.take(_).sum) // cumulative sum of the percentage values
        // island ID + 1 is used for the mapping to work as expected when configured amounts are regarded as percentages
        i => sums.indexWhere((i + 1).toDouble / islands * 100 <= _) // return the mapping function
      }

      /** A factory for IslandToConfigurationMapping factory functions
        *
        * @param asPercentage a boolean value to choose between asAmount or asPercentage factories
        * @return an asPercentage factory if asPercentage is true, an asAmount factory otherwise
        */
      def apply(percentage: Boolean = false): IslandToConfigurationMappingFactory =
        if (percentage) asPercentage _ else asAmount _
    }

    /** Factory to build a collection of elements, one per island, using parameters collected
      * from the island configurations. It takes into account duplicates (elements with the same parameters
      * are created only once) and supports different mapping from island IDs to configurations. Its main purpose
      * is being used to build strategy factories although is general enough to be used for other purposes as well.
      *
      * @param islands the number of islands
      * @param conf the island configurations
      * @param collect a function to collect parameters from an island configuration
      * @param create a function to create instances of F from collected parameters
      * @tparam T the type of the collected parameters
      * @tparam F the type of the elements of the returned collection
      * @return a collection of elements created using parameters collected from the island configurations
      */
    def apply[T, F](islands: Int, conf: PSOIslandsConfiguration)(
      collect: PSOIslandParameters => T)(create: T => F): IndexedSeq[F] = {
      // collect the information from the islands configurations
      val (amounts, params) = conf.islands.map(island => (island.amount, collect(island))).unzip
      // get the mapping function to map islands IDs to configurations
      def mapping = IslandToConfigurationMapping(conf.asPercentage)(islands, amounts)
      // create the elements from the parameters without duplicates
      val unique = (0 until islands).map(i => params(mapping(i))).distinct
      val elems = unique.map(p => create(p))
      // return a collection with the elements for all the islands (adding references for duplicates)
      (0 until islands).map(i =>
        elems(unique.indexOf(params(mapping(i)))))
    }
  }
}
