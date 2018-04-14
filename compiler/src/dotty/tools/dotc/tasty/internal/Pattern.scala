package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

import scala.tasty.trees
import scala.tasty.types

object Pattern {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.Pattern = Impl(tree, ctx)

  def unapplyValue(tree: scala.tasty.Tree): Option[trees.Value.Data] = tree match {
    case Impl(lit: tpd.Literal, ctx) => Some(Term(lit)(ctx))
    case Impl(ident: tpd.Ident, ctx) => Some(Term(ident)(ctx))
    case _ => None
  }

  def unapplyBind(tree: scala.tasty.Tree): Option[trees.Bind.Data] = tree match {
    case Impl(Trees.Bind(name: Names.TermName, body), ctx) => Some(TermName(name), Pattern(body)(ctx))
    case _ => None
  }

  def unapplyUnapply(tree: scala.tasty.Tree): Option[trees.Unapply.Data] = tree match {
    case Impl(Trees.UnApply(fun, implicits, patterns), ctx) => Some((Term(fun)(ctx), implicits.map(Term(_)(ctx)), patterns.map(Pattern(_)(ctx))))
    case _ => None
  }

  def unapplyAlternative(tree: scala.tasty.Tree): Option[trees.Alternative.Data] = tree match {
    case Impl(Trees.Alternative(patterns), ctx) => Some(patterns.map(Pattern(_)(ctx)))
    case _ => None
  }

  def unapplyTypeTest(tree: scala.tasty.Tree): Option[trees.TypeTest.Data] = tree match {
    case Impl(Trees.Typed(_, tpt), ctx) => Some(TypeTree(tpt)(ctx))
    case _ => None
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends trees.Pattern with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case trees.Value(v) => s"Value($v)"
        case trees.Bind(name, body) => s"Bind($name, $body)"
        case trees.Unapply(fun, implicits, patterns) => s"Unapply($fun, ${list(implicits)}, ${list(patterns)})"
        case trees.Alternative(patterns) => s"Alternative(${list(patterns)})"
        case trees.TypeTest(tpt) => s"TypeTest($tpt)"
      }
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}

