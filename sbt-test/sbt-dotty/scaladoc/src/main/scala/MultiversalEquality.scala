package example

import scala.language.strictEquality

/**
  * Multiversal Equality: https://nightly.scala-lang.org/docs/reference/contextual/multiversal-equality.html
  * scala.Eq definition: https://github.com/scala/scala3/blob/master/library/src/scala/CanEqual.scala
  */
object MultiversalEquality {

  def test: Unit = {

    // Values of types Int and String cannot be compared with == or !=,
    // unless we add the derived delegate instance like:
    given CanEqual[Int, String] = CanEqual.derived
    println(3 == "3")

    // By default, all numbers are comparable, because of;
    // implicit def eqlNumber: CanEqual[Number, Number] = derived
    println(3 == 5.1)

    // By default, all Sequences are comparable, because of;
    // implicit def eqlSeq[T, U](implicit eq: CanEqual[T, U]): CanEqual[GenSeq[T], GenSeq[U]] = derived
    println(List(1, 2) == Vector(1, 2))

    class A(a: Int)
    class B(b: Int)

    val a = new A(4)
    val b = new B(4)

    // scala.language.strictEquality is enabled, therefore we need some extra delegate instances
    // to compare instances of A and B.
    given CanEqual[A, B] = CanEqual.derived
    given CanEqual[B, A] = CanEqual.derived

    println(a != b)
    println(b == a)
  }
}
