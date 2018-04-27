trait Base
trait TypeConstr[X]

class X1[A >: _ | X1[_]] // error
class X2[A >: _ & X2[_]] // error
class X3[A >: X3[_] | _] // error
class X4[A >: X4[_] & _] // error

class A1 extends _ // error
class A2 extends _ with _ // error // error
class A3 extends Base with _ // error
class A4 extends _ with Base // error

object Test {
  type T1 = _ // error
  type T2 = _[Int] // error
  type T3 = _ { type S } // error
  type T4 = [X] => _ // error

  // Open questions:
  type T5 = TypeConstr[_ { type S }] // error
  type T5 = TypeConstr[_[Int]] // error
}
