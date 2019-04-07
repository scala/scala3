package scala.quoted.matching

import scala.quoted.Expr

import scala.tasty.Reflection // TODO do not depend on reflection directly

/** Matches a literal sequence of expressions */
object Repeated {

  def unapply[T](expr: Expr[Seq[T]])(implicit reflect: Reflection): Option[Seq[Expr[T]]] = {
    import reflect.{Repeated => RepeatedTree, _} // TODO rename to avoid clash
    def repeated(tree: Term): Option[Seq[Expr[T]]] = tree match {
      case Typed(RepeatedTree(elems, _), _) => Some(elems.map(x => x.seal.asInstanceOf[Expr[T]]))
      case Block(Nil, e) => repeated(e)
      case Inlined(_, Nil, e) => repeated(e)
      case _  => None
    }
    repeated(expr.unseal)
  }

}
