trait T
class B extends T
class C extends T

object A {
  implied b for B
  implied c for C
}

object Test extends App {
  import A._
  import implied A.{for B}

  val x: B = b // OK
  println(c) // error: not found

  the[C] // error

}