// This file is part of tests for inferring GADT constraints from type members,
// which needed to be reverted because of soundness issues.
//
// Lines with "// limitation" are the ones that we could soundly allow.
object Test {

  trait Expr { type T }
  trait IntLit extends Expr { type T <: Int }
  trait IntExpr extends Expr { type T = Int }

  type ExprSub[+A] = Expr { type T <: A }
  type ExprExact[A] = Expr { type T = A }

  trait IndirectIntLit extends Expr { type S <: Int; type T = S }
  trait IndirectIntExpr extends Expr { type S = Int; type T = S }

  type IndirectExprSub[+A] = Expr { type S <: A; type T = S }
  type IndirectExprSub2[A] = Expr { type S = A; type T <: S }
  type IndirectExprExact[A] = Expr { type S = A; type T = S }

  trait AltIndirectIntLit extends Expr { type U <: Int; type T = U }
  trait AltIndirectIntExpr extends Expr { type U = Int; type T = U }

  type AltIndirectExprSub[+A] = Expr { type U <: A; type T = U }
  type AltIndirectExprSub2[A] = Expr { type U = A; type T <: U }
  type AltIndirectExprExact[A] = Expr { type U = A; type T = U }

  def foo[A](e: ExprExact[A]) = e match {
    case _: IndirectIntLit =>
      val a: A = 0 // error
      val i: Int = ??? : A // limitation // error

    case _: IndirectExprSub[Int] =>
      val a: A = 0 // error
      val i: Int = ??? : A // limitation // error

    case _: IndirectExprSub2[Int] =>
      val a: A = 0 // error
      val i: Int = ??? : A // limitation // error

    case _: IndirectIntExpr =>
      val a: A = 0 // limitation // error
      val i: Int = ??? : A // limitation // error

    case _: IndirectExprExact[Int] =>
      val a: A = 0 // limitation // error
      val i: Int = ??? : A // limitation // error
  }

  def bar[A](e: ExprSub[A]) = e match {
    case _: IndirectIntLit =>
      val a: A = 0 // error
      val i: Int = ??? : A // error

    case _: IndirectExprSub[Int] =>
      val a: A = 0 // error
      val i: Int = ??? : A // error

    case _: IndirectExprSub2[Int] =>
      val a: A = 0 // error
      val i: Int = ??? : A // error

    case _: IndirectIntExpr =>
      val a: A = 0 // limitation // error
      val i: Int = ??? : A // error

    case _: IndirectExprExact[Int] =>
      val a: A = 0 // limitation // error
      val i: Int = ??? : A // error
  }
}
