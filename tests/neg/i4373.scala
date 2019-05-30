trait Base
trait TypeConstr[X]

class X1[A >: _ | X1[_]] // error
class X2[A >: _ & X2[_]] // error
class X3[A >: X3[_] | _] // error
class X4[A >: X4[_] & _] // error
class X5[A >: _ with X5[_]] // error
class X6[A >: X6[_] with _] // error

class A1 extends _ // error
class A2 extends _ with _ // error // error
class A3 extends Base with _ // error
class A4 extends _ with Base // error

object Test {
  type T1 = _ // error
  type T2 = _[Int] // error
  type T3 = _ { type S } // error
  type T4 = [X] =>> _ // error

  // Open questions:
  type T5 = TypeConstr[_ { type S }] // error
  type T6 = TypeConstr[_[Int]] // error

  // expression types
  type T7 = (=> Int) | (Int => Int) // error
  type T8 = (=> Int) & (Int => Int) // error
  type T9 = (=> Int) with (Int => Int) // error
  type T10 = (Int => Int) | (=> Int) // error
  type T11 = (Int => Int) & (=> Int) // error
  type T12 = (Int => Int) with (=> Int) // error

  // annotations
  type T13 = _ @ annotation.tailrec // error
  type T14 = Int @ _ // error
  type T15 = (_ | Int) @ annotation.tailrec // error
  type T16 = (Int | _) @ annotation.tailrec // error
  type T17 = Int @ (_ | annotation.tailrec) // error
  type T18 = Int @ (annotation.tailrec | _) // error

  type T19 = (_ with Int) @ annotation.tailrec // error
  type T20 = (Int with _) @ annotation.tailrec // error
  type T21 = Int @ (_ with annotation.tailrec) // error
  type T22 = Int @ (annotation.tailrec with _) // error

  type T23 = (_ & Int) @ annotation.tailrec // error
  type T24 = (Int & _) @ annotation.tailrec // error
  type T25 = Int @ (_ & annotation.tailrec) // error
  type T26 = Int @ (annotation.tailrec & _) // error
}
