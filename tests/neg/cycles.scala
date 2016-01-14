class Foo[T <: U, U <: T] // error: illegal cyclic reference: upper bound U of type T refers back to the type itself

class Bar[T >: T] // error: illegal cyclic reference: lower bound T of type T refers back to the type itself


class A {
  val x: T = ???
  type T <: x.type // error: cyclic reference involving value x
}

class B {
  type T <: x.type // error: illegal cyclic reference: upper bound B.this.T(B.this.x) of type T refers back to the type itself
  val x: T = ???
}

class C {
  val x: D#T = ???
  class D {
    type T <: x.type // error: cyclic reference involving value x
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
  type X = (U, U) // error: illegal cyclic reference: alias (T1.this.U, T1.this.U) of type X refers back to the type itself
  type U = X & Int
}
class T2 {
  type X = (U, U) // error: illegal cyclic reference: alias (T2.this.U, T2.this.U) of type X refers back to the type itself
  type X = (U, U) // error: X is already defined as type X
  type U = X | Int
}
object T12 {
  ??? : (T1 {})#U
  ??? : (T2 {})#U
}
