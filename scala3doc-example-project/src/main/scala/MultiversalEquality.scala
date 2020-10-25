package example

import scala.language.strictEquality

/**
  * Multiversal Equality: https://dotty.epfl.ch/docs/reference/contextual/multiversal-equality.html
  * scala.Eq definition: https://github.com/lampepfl/dotty/blob/master/library/src/scala/Eql.scala
  */
object MultiversalEquality {

  def test: Unit = {

    // Values of types Int and String cannot be compared with == or !=,
    // unless we add the derived delegate instance like:
    given Eql[Int, String] = Eql.derived
    println(3 == "3")

    // By default, all numbers are comparable, because of;
    // implicit def eqlNumber: Eql[Number, Number] = derived
    println(3 == 5.1)

    // By default, all Sequences are comparable, because of;
    // implicit def eqlSeq[T, U](implicit eq: Eql[T, U]): Eql[GenSeq[T], GenSeq[U]] = derived
    println(List(1, 2) == Vector(1, 2))

    class A(a: Int)
    class B(b: Int)

    val a = new A(4)
    val b = new B(4)

    // scala.language.strictEquality is enabled, therefore we need some extra delegate instances
    // to compare instances of A and B.
    given Eql[A, B] = Eql.derived
    given Eql[B, A] = Eql.derived

    println(a != b)
    println(b == a)
  }
}
