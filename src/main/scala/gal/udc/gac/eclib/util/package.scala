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

import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.concurrent.duration._

package object util {
  
  object Random extends LazyLogging {
    import org.apache.commons.math3.random.{MersenneTwister, SynchronizedRandomGenerator, RandomDataGenerator}
    import org.apache.commons.math3.distribution.{UniformRealDistribution, UniformIntegerDistribution}

//    private lazy val seed = (new RandomDataGenerator).nextSecureLong(Long.MinValue, Long.MaxValue)
//    private lazy val random = new SynchronizedRandomGenerator(new MersenneTwister(seed))
		private lazy val random = new SynchronizedRandomGenerator(new MersenneTwister)
    lazy val generator = new RandomDataGenerator(random)
    lazy val reals = new UniformRealDistribution(random, 0.0, 1.0)
    lazy val integers = new UniformIntegerDistribution(random, 0, 1) // Binary uniform distribution

    /** This function samples n distinct integers from a given uniform integer distribution
		 *  
		 * @param n the number of distinct integers to sample
		 * @param distribution the uniform integer distribution
		 * @return the random samples
		 */   
		def sampleN(n: Int, distribution: UniformIntegerDistribution): Array[Int] = {

				/** Inner recursive function that samples n integers from an uniform integer distribution
				 *  taking account of duplicates
				 *  
				 * @param samples used to store the samples between recursive calls 
				 * @return the random samples
				 */
				@tailrec
				def sampleN_(samples: Array[Int] = Array()): Array[Int] = 
				if (samples.size == n) samples
				else { 
					logger.trace(s"[#${samples.size}] samples: ${samples.mkString("(", ", ", ")")}")
					sampleN_((samples ++: distribution.sample(n - samples.size)).distinct)
				}

				sampleN_()
		}	
  }

  def duration[R](code: => R) = {  // measure code execution time
    val t1 = Deadline.now
    (code, Deadline.now - t1)
  }

  def duration(code: => Unit) = {  // measure code execution time
    val t1 = Deadline.now
    code
    Deadline.now - t1
  }
}