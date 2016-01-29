class Foo[T <: U, U <: T] // error

class Bar[T >: T] // error

class A {
  val x: T = ???
  type T <: x.type // error
}

class B {
  type T <: x.type // error
  val x: T = ???
}

class C {
  val x: D#T = ???
  class D {
    type T <: x.type // error
    val z: x.type = ???
  }
}

class E {
  class F {
    type T <: x.type
    val z: x.type = ??? // error
  }
  lazy val x: F#T = ???
}

class T1 {
  type X = (U, U) // error
  type U = X & Int
}
class T2 {
  type X = (U, U) // error
  type U = X | Int
}
object T12 {
  ??? : (T1 {})#U
  ??? : (T2 {})#U
}
