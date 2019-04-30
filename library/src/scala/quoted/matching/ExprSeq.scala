package scala.quoted.matching

import scala.quoted.Expr

import scala.tasty.Reflection // TODO do not depend on reflection directly

/** Literal sequence of expressions */
object ExprSeq {

  /** Matches a literal sequence of expressions */
  def unapply[T](expr: Expr[Seq[T]])(implicit reflect: Reflection): Option[Seq[Expr[T]]] = {
    import reflect._
    def rec(tree: Term): Option[Seq[Expr[T]]] = tree match {
      case Typed(Repeated(elems, _), _) => Some(elems.map(x => x.seal.asInstanceOf[Expr[T]]))
      case Block(Nil, e) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.unseal)
  }

}
