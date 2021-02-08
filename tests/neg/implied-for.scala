trait T
class B extends T
class C extends T

object A {
  given b: B with {}
  given c: C with {}
}

object Test extends App {
  import A.*
  import A.{given B}

  val x: B = b // OK
  println(c) // error: not found

  summon[C] // error

}