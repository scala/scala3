package example

import scala.language.existentials
import scala.language.higherKinds
import scala.language.reflectiveCalls

class AdvC[T] {
  def t: T = ???
}

class Structural {
  def s1: { val x: Int } = ???
  def s2 = new { val x: Int = ??? }
  def s3 = new { def m(x: Int): Int = ??? }
}

class Existential {
  def e1: List[_] = ???
}

class AdvD[CC[B]] extends AdvC[CC[B]]

object AdvTest {
  val s = new Structural
  val s1 = s.s1
  val s2 = s.s2
  val s3 = s.s3

  val e = new Existential
  val e1 = e.e1
  val e1x = e.e1.head
  locally {
    (??? : Any) match {
      case e3: List[_] =>
        val e3x = e3.head
        ()
    }
  }
}
