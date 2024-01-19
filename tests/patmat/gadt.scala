object Test {
  sealed trait Expr[T]
  case class IntLit(i: Int) extends Expr[Int]
  case class BooleanLit(b: Boolean) extends Expr[Boolean]
  case class Sum(l: Expr[Int], r: Expr[Int]) extends Expr[Int]
  case class Or(l: Expr[Boolean], r: Expr[Boolean]) extends Expr[Boolean]

  def foo1a(x: Expr[Int]) = x match {
    case _: IntLit => true
    case _: Sum => true
  }

  def foo1b(x: Expr[Int]) = x match {
    case _: Sum => true
  }

  def foo2a(x: Expr[Boolean]) = x match {
    case _: BooleanLit => true
    case _: Or => true
  }

  def foo2b(x: Expr[Boolean]) = x match {
    case _: BooleanLit => true
  }

  def foo3a(x: Expr[Boolean]) = x match {
    case _: BooleanLit => true
    case _: Or => true
    // case _: Sum => true
  }

  def foo3b(x: Expr[Int]) = x match {
    case _: IntLit => true
    case _: Sum => true
    // case _: Or => true
  }

  def foo4a(x: Expr[?]) = x match {
    case _: IntLit => true
    case _: Sum => true
    case _: BooleanLit => true
    case _: Or => true
  }

  def foo4b(x: Expr[?]) = x match {
    case _: Sum => true
    case _: Or => true
  }

  def foo5a[T <: Int](x: Expr[T]) = x match {
    case _: IntLit => true
    case _: Sum => true
  }

  def foo5b[T <: Int](x: Expr[T]) = x match {
    case _: IntLit => true
  }
}
