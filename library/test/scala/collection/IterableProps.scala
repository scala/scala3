package scala.collection

import org.scalacheck.*
import Prop.*
import Gen.*

object IterableProps extends Properties("Iterable.scanLeft") {
  property("scanLeft") = forAll { (xs: List[Int], z: Int) => {
    val sums = xs.scanLeft(z)(_ + _)
    (xs.size == 0) || sums.zip(sums.tail).map(x => x._2 - x._1) == xs
  }}
}






