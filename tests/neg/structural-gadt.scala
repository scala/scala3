// This file is part of tests for inferring GADT constraints from type members,
// which needed to be reverted because of soundness issues.
//
// Lines with "// limitation" are the ones that we could soundly allow.
object Test {

  trait Expr { type T }
  trait IntLit extends Expr { type T <: Int }
  trait IntExpr extends Expr { type T = Int }

  def foo[A](e: Expr { type T = A }) = e match {
    case _: IntLit =>
      val a: A = 0 // error
      val i: Int = ??? : A // limitation // error

    case _: Expr { type T <: Int } =>
      val a: A = 0 // error
      val i: Int = ??? : A // limitation // error

    case _: IntExpr =>
      val a: A = 0 // limitation // error
      val i: Int = ??? : A // limitation // error

    case _: Expr { type T = Int } =>
      val a: A = 0 // limitation // error
      val i: Int = ??? : A // limitation // error
  }

  def bar[A](e: Expr { type T <: A }) = e match {
    case _: IntLit =>
      val a: A = 0 // error
      val i: Int = ??? : A // error

    case _: Expr { type T <: Int } =>
      val a: A = 0 // error
      val i: Int = ??? : A // error

    case _: IntExpr =>
      val a: A = 0 // limitation // error
      val i: Int = ??? : A // error

    case _: Expr { type T = Int } =>
      val a: A = 0 // limitation // error
      val i: Int = ??? : A // error
  }

  trait IndirectExprOfIntList extends Expr {
    type T = U
    type U <: List[Int]
  }
  def baz[A](e: Expr { type T <: List[A] }) = e match {
    case _: IndirectExprOfIntList =>
      val a: A = 0 // error
      val i: Int = ??? : A // error
  }
}
