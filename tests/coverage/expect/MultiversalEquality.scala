package example

import scala.language.strictEquality

/**
  * Multiversal Equality: https://dotty.epfl.ch/docs/reference/contextual/multiversal-equality.html
  * scala.CanEqual definition: https://github.com/lampepfl/dotty/blob/master/library/src/scala/CanEqual.scala
  * Taken from https://github.com/scala/scala3-example-project
  */
object MultiversalEquality:

  def test(): Unit =
    given CanEqual[Int, String] = CanEqual.derived
    println(3 == "3")

    println(3 == 5.1)

    println(List(1, 2) == Vector(1, 2))

    class A(a: Int)
    class B(b: Int)

    val a = A(4)
    val b = B(4)

    given CanEqual[A, B] = CanEqual.derived
    given CanEqual[B, A] = CanEqual.derived

    println(a != b)
    println(b == a)

