class Foo[T <: U, U <: T]

class Bar[T >: T]

class A {
  val x: T = ???
  type T <: x.type
}

class B {
  type T <: x.type
  val x: T = ???
}

class C {
  val x: D#T = ???
  class D {
    type T <: x.type
    val z: x.type = ???
  }
}

class E {
  class F {
    type T <: x.type
    val z: x.type = ???
  }
  val x: F#T = ???
}

class T1 {
  type X = (U, U) // cycle
  type U = X & Int
}
class T2 {
  type X = (U, U) // cycle
  type U = X | Int
}
object T12 {
  ??? : (T1 {})#U
  ??? : (T2 {})#U
}
