class Foo[T <: U, U <: T] // error: cycle

class Bar[T >: T] // error: cycle

class A {
  val x: T = ???
  type T <: x.type // error: cycle
}

class B {
  type T <: x.type // error: cycle
  val x: T = ???
}

class C {
  val x: D#T = ???
  class D {
    type T <: x.type // error: cycle
    val z: x.type = ???
  }
}

class E {
  class F {
    type T <: x.type // error: not stable
    val z: x.type = ??? // error: not stable
  }
  lazy val x: F#T = ???
}

class T1 {
  type X = (U, U) // error: cycle
  type U = X & Int
}
class T2 {
  type X = (U, U) // error: cycle
  type U = X | Int
}
object T12 {
  ??? : (T1 {})#U
  ??? : (T2 {})#U
}
