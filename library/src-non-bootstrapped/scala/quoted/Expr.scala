package scala.quoted

abstract class Expr[+T] private[scala]:
  def unseal(using s: Scope): s.tasty.Term
