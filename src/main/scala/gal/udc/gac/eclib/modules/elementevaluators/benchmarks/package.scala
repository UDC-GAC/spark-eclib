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
package gal.udc.gac.eclib.modules.elementevaluators

import gal.udc.gac.eclib.searchspace.{Element, FitnessFunction}

package object benchmarks {

  object BaseFunctions {
    def apply(name: String, files:Option[String] = None): Option[FitnessFunction] = name match {
      case "Griewank" => Some(Griewank())
      case "Schaffer_f6" => Some(Schaffer_f6())
      case "Schaffer_N2" => Some(Schaffer_N2())
      case "Schaffer_N4" => Some(Schaffer_N4())
      case _ => LSGO2013(name)  // try the base functions in the LSGO2013 library
    }
  }

  // Adaptor for the LSGO2013 benchmark functions in https://github.com/xoanpardo/scala-lsgo-benchmarks
  object LSGO2013 {

    // workaround to avoid serialization errors in Spark caused by
    // the original LSGO2013 benchmark functions not being serializable
    // see: https://www.nicolaferraro.me/2016/02/22/using-non-serializable-objects-in-apache-spark/
    private object Wrapper {

      import gal.udc.gac.lsgo2013.Benchmark._

      // note: using Fx() instead of Fx()(_) throws a NotSerializable exception
      def f1: FitnessFunction = F1()(_)
      def f2: FitnessFunction = F2()(_)
      def f3: FitnessFunction = F3()(_)
      def f4: FitnessFunction = F4()(_)
      def f5: FitnessFunction = F5()(_)
      def f6: FitnessFunction = F6()(_)
      def f7: FitnessFunction = F7()(_)
      def f8: FitnessFunction = F8()(_)
      def f9: FitnessFunction = F9()(_)
      def f10: FitnessFunction = F10()(_)
      def f11: FitnessFunction = F11()(_)
      def f12: FitnessFunction = F12()(_)
      def f13: FitnessFunction = F13()(_)
      def f14: FitnessFunction = F14()(_)
      def f15: FitnessFunction = F15()(_)
    }

    import gal.udc.gac.lsgo2013._

    def apply(name: String): Option[FitnessFunction] = name match {
        // base functions
        case "Sphere" => Some(Sphere())
        case "Elliptic" => Some(Elliptic())
        case "Rastrigin" => Some(Rastrigin())
        case "Ackley" => Some(Ackley())
        case "Schwefel" => Some(Schwefel())
        case "Rosenbrock" => Some(Rosenbrock())
        // LSGO2013 functions
        case "LSGO2013_F1" => Some(Wrapper.f1)
        case "LSGO2013_F2" => Some(Wrapper.f2)
        case "LSGO2013_F3" => Some(Wrapper.f3)
        case "LSGO2013_F4" => Some(Wrapper.f4)
        case "LSGO2013_F5" => Some(Wrapper.f5)
        case "LSGO2013_F6" => Some(Wrapper.f6)
        case "LSGO2013_F7" => Some(Wrapper.f7)
        case "LSGO2013_F8" => Some(Wrapper.f8)
        case "LSGO2013_F9" => Some(Wrapper.f9)
        case "LSGO2013_F10" => Some(Wrapper.f10)
        case "LSGO2013_F11" => Some(Wrapper.f11)
        case "LSGO2013_F12" => Some(Wrapper.f12)
        case "LSGO2013_F13" => Some(Wrapper.f13)
        case "LSGO2013_F14" => Some(Wrapper.f14)
        case "LSGO2013_F15" => Some(Wrapper.f15)
        case _ => None
      }
  } // LSGO2013

  object BenchmarkFunction {
    def apply(name: String, files:Option[String] = None): Option[FitnessFunction] =
      BaseFunctions(name) orElse LSGO2013(name)
  }
}
