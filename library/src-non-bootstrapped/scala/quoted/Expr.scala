package scala.quoted

abstract class Expr[+T] private[scala]

object Expr:
  def unapply[T](x: Expr[T])(using Unliftable[T])(using Quotes): Option[T] = ???
