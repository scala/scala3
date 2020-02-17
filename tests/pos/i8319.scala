package example

sealed abstract class Tree[T]
case class Lam[T,U]() extends Tree[Any]
case class App[T,U]() extends Tree[Any]
case class Var()      extends Tree[Any]

object Branch:
  def unapply(branch: Lam[?,?] | App[?,?]): true = true

private def foo(s: Option[Tree[?]]) = s match // seems to only occur in a nested pattern
  case Some(_: Var)   => true // must come first
  case Some(Branch()) => true // must be unapply and not direct type check
  case _              => false