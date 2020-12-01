package scala.quoted

abstract class Expr[+T] private[scala]

object Expr:
  def apply[T](x: T)(using ToExpr[T])(using Quotes): Expr[T] = ???
  def unapply[T](x: Expr[T])(using FromExpr[T])(using Quotes): Option[T] = ???
