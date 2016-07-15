object PatmatADT {
  abstract sealed class Odd(x: Odd)

  case class Good(x: Odd) extends Odd(x)
  case class Bad(x: Odd) extends Odd(x)

  def foo1a(x: Odd) = x match { // warning: Good(_: Bad), Bad(_: Good)
    case Good(_: Good) => false
    case Bad(_: Bad) => false
  }

  def foo1b(x: Odd) = x match {
    case Good(_: Good) => false
    case Bad(_: Bad) => false
    case Good(_: Bad) => false
    case Bad(_: Good) => false
  }

  def foo2(x: Option[Int]) = x match { // warning: Some(_: Int)
    case Some(_: Double) => true
    case None => true
  }

  def foo3a[T](x: Option[T]) = (x, x) match { // warning: (Some(_), Some(_)), (None, Some(_))
    case (Some(_), None) => true
    case (None, None) => true
  }

  def foo3b[T](x: Option[T]) = (x, x) match { // warning: (Some(_), Some(_)), (None, None)
    case (Some(_), None) => true
    case (None, Some(_)) => true
  }

  sealed trait Base
  case class Foo() extends Base

  def foo4(x: Base) = x match {
    case Foo() =>
  }

  sealed abstract class CL3Literal
  case object IntLit extends CL3Literal
  case object CharLit extends CL3Literal
  case object BooleanLit extends CL3Literal


  sealed abstract class Tree
  case class LetL(value: CL3Literal) extends Tree

  def foo5(tree: Tree) : Any = tree match {
    case LetL(CharLit) =>
  }

  def foo6[T](l: List[T]): Boolean = l match {
    case x::xs => true
    case Nil => false
  }
}