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

package object searchspace {

  /** Search space points ------------------------------------------------------------------------- */

  val BAD_POINT = Double.MaxValue  // used to indicate null or not valid points

  type Point   = Double            // a point in the search space
  object Point {
    val Zero = 0.0
  }

  type Element = Vector[Point]  // a vector in the search space
  type Position = Element // an alias for an element
  object Element {
    object Zero {
      def apply(dim: Int): Element = Vector.fill(dim)(Point.Zero)
    }
    def apply(p: Point*): Element = p // factory method to build an element from a sequence of points
  }
  /** implicit to add the combine method to Element */
  implicit class ElementOps(e: Element) {
    /** Combine two elements with the provided function
      *
      * @param ee, the second element to be combined
      * @param f, the function to combine both elements
      * @return the element resulting from the combination
      * @note the elements to be combined must have the same size
      */
    def combine(ee: Element)(f: (Point, Point) => Point): Element = {
      require(e.size == ee.size, "Elements to be combined must have the same size")
      e.zip(ee) map (pp => f(pp._1, pp._2))
    }
  }

  implicit def toElement(v: Vector[Point]): Element = v
  implicit def toElement(s: Seq[Point]): Element = s.toVector  // implicit conversion from Seq[Point] to Element

  type GenerationFunction = () => Element      // an element generator function type
  type FitnessFunction  = (Element) => Double  // a fitness function type

  /** Search space bounds ------------------------------------------------------------------------- */

  type Bound            = Vector[Point]        // a bounding vector in the search space
  val EMPTY_BOUND       = Vector[Point]()      // an empty bound

  /**
   * Function type to bound a Point to upper and lower limits:
   * (point, low bound, upper bound) => bounded point
   **/
  type BoundFunction = (Point, Point, Point) => Point

  /** Bounding functions */
  object BoundFunction {

    /**
     * Default bound function
     * @return a bounding function that bounds a point inside search space limits
     * @note this function corresponds to DE_correction_bounds2 in SaCess
     **/
    def default: BoundFunction = (x: Point, l: Point, u: Point) =>
      if (l == u || x < l) l
      else if (x > u) u
      else x

    /**
     * Scale a sample inside the lower and upper limits
     * @return a bounding function that scales an uniform sample in [0.0, 1.0) inside search space limits
     **/
    def scale: BoundFunction = (x: Point, l: Point, u: Point) => l + x * (u - l)
    //    def scale: BoundFunction = (x: Point, l: Point, u: Point) => x   // for testing

    /**
     * HyperRectangles bounding function
     * @return a bounding function like the one used in the Scatter Search HyperRectangles strategy
     * TODO??: move to scattersearch
     **/
    def hyperbound: BoundFunction = { (x: Point, l: Point, u: Point) =>

      val pBound = 0.5 // default value for user prob_bound option in SaCess. TODO??: define it as an input parameter

      def probBound(p: Point, l: Point): Point = {
        // bound the point only if greater than a random sample
        import gal.udc.gac.eclib.util.Random.reals
        if (reals.sample > pBound) l
        else p
      }

      if (x < l) probBound(x, l)
      else if (x > u) probBound(x, u)
      else x
    }

    /**
     * Normalize a point inside search space limits
     * @return a bounding function that normalizes a point inside search space limits
     * @note this bounding function is taken from the FORTRAN implementation of SaCess.
     *       In the original code there is an additional condition {{{ math.abs(l) + math.abs(u) == 0 }}}
     *       but it makes no much sense, because it already holds when l == u, so it has not been included here
     **/
    def normalize: BoundFunction = (x: Point, l: Point, u: Point) =>
      if (x == l || l == u) 0.0
      else x / (u - l)
  }

  /**
   * Implicit to add a bound method to Element
   **/
  implicit def toBoundedElement(e: Element) = new {
    // bound an element to search space limits using given bound function
    def bound(f: BoundFunction)(params: SearchSpaceParameters): Element = {
      if (params.lowerLimits.isEmpty) e
      else (e, params.lowerLimits.get, params.upperLimits.get).zipped.map((x, l, u) => f(x, l, u))
    }
  }

  /** Search space distances ------------------------------------------------------------------------- */

  type Distance = Double
  type Distances = Vector[Distance]

  type DistanceFunction = (Element, Element) => Distance

  /** Distance functions */
  object DistanceFunction {

    /**
      * Euclidean distance: square root of the sum of squares
      *
      *  @pre a and b have the same length
      */
    def euclidean(a: Element, b: Element): Distance =
      math.sqrt(a.combine(b)((x, y) => math.pow((x - y), 2)).sum)

  }
}
