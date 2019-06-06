object Test {
  trait Expr[T]
  trait IntExpr extends Expr[Int]
  class Const[T] extends Expr[T]
  final class Fin

  def foo1[T](x: Unit | Const[T]): T = x match {
    case _: IntExpr => 0
  }

  def bar1[T](x: Const[T]): T = x match {
    case _: (Unit | IntExpr) => 0
  }

  def foo2[T](x: Fin | Const[T]): T = x match {
    case _: IntExpr => 0
  }

  def bar2[T](x: Const[T]): T = x match {
    case _: (Fin | IntExpr) => 0
  }
}
