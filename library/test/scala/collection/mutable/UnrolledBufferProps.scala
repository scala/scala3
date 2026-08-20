package scala.collection.mutable

import org.scalacheck.*
import Prop.*
import Gen.*

object UnrolledBufferProps extends Properties("UnrolledBuffer") {

  property("concat size") = forAll { (l1: List[Int], l2: List[Int]) =>
    val u1 = new UnrolledBuffer[Int]
    u1 ++= l1
    val u2 = new UnrolledBuffer[Int]
    u2 ++= l2
    val totalsz = u1.size + u2.size
    u1 concat u2
    totalsz == u1.size
  }

  property("adding") = forAll { (l: List[Int]) =>
    val u = new UnrolledBuffer[Int]
    u ++= l
    u == l
  }

}

