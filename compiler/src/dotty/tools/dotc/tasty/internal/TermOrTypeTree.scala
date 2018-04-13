package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.terms
import scala.tasty.typetrees

object TermOrTypeTree {

  // compat.TermOrTypeTree is used where Term | TypeTree should go in dotty
  def apply(arg: tpd.Tree)(implicit ctx: Context): scala.util.Either[terms.Term, typetrees.TypeTree] =
    if (!arg.isType) Left(Term(arg)) else Right(TypeTree(arg))

}
