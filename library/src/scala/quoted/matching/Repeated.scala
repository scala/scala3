package scala.quoted.matching

import scala.quoted.Expr

import scala.tasty.Reflection // TODO do not depend on reflection directly

/** Matches a sequence of expressions */
object Repeated {

  def unapply[T](expr: Expr[Seq[T]])(implicit reflect: Reflection): Option[Seq[Expr[T]]] = {
    import reflect._
    def repeated(tree: Term): Option[Seq[Expr[T]]] = tree match {
      case Term.Repeated(elems, _) => Some(elems.map(x => x.seal.asInstanceOf[Expr[T]]))
      case Term.Block(Nil, e) => repeated(e)
      case Term.Inlined(_, Nil, e) => repeated(e)
      case _  => None
    }
    repeated(expr.unseal)
  }

}
