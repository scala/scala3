trait T
class B extends T
class C extends T

object A {
  delegate b for B
  delegate c for C
}

object Test extends App {
  import A._
  import delegate A.{for B}

  val x: B = b // OK
  println(c) // error: not found

  the[C] // error

}