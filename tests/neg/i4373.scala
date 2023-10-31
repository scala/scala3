trait Base
trait TypeConstr[X]

class X1[A >: ? | X1[?]] // error
class X2[A >: ? & X2[?]] // error
class X3[A >: X3[?] | ?] // error
class X4[A >: X4[?] & ?] // error
class X5[A >: ? with X5[?]] // error
class X6[A >: X6[?] with ?] // error

class A1 extends ? // error
class A2 extends ? with ? // error // error
class A3 extends Base with ? // error
class A4 extends ? with Base // error

object Test {
  type T1 = ? // error
  type T2 = ?[Int] // error
  type T3 = ? { type S } // error
  type T4 = [X] =>> ? // error

  // Open questions:
  type T5 = TypeConstr[? { type S }] // error
  type T6 = TypeConstr[?[Int]] // error

  // expression types
  type T7 = (=> Int) | (Int => Int) // error
  type T8 = (=> Int) & (Int => Int) // error
  type T9 = (=> Int) with (Int => Int) // error
  type T10 = (Int => Int) | (=> Int) // error
  type T11 = (Int => Int) & (=> Int) // error
  type T12 = (Int => Int) with (=> Int) // error

  // annotations
  type T13 = ? @ annotation.tailrec // error
  type T14 = Int @ ? // error
  type T15 = (? | Int) @ annotation.tailrec // error
  type T16 = (Int | ?) @ annotation.tailrec // error
  type T17 = Int @ (? | annotation.tailrec) // error
  type T18 = Int @ (annotation.tailrec | ?) // error

  type T19 = (? with Int) @ annotation.tailrec // error
  type T20 = (Int with ?) @ annotation.tailrec // error
  type T21 = Int @ (? with annotation.tailrec) // error
  type T22 = Int @ (annotation.tailrec with ?) // error

  type T23 = (? & Int) @ annotation.tailrec // error
  type T24 = (Int & ?) @ annotation.tailrec // error
  type T25 = Int @ (? & annotation.tailrec) // error
  type T26 = Int @ (annotation.tailrec & ?) // error
}
