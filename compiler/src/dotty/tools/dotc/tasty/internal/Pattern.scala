package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.StdNames._

import scala.tasty.patterns
import scala.tasty.types

object Pattern {

  def apply(tree: tpd.Tree)(implicit ctx: Context): patterns.Pattern = Impl(tree, ctx)

  def unapplyValue(arg: patterns.Pattern): Option[patterns.Value.Data] = arg match {
    case Impl(lit: tpd.Literal, ctx) => Some(Term(lit)(ctx))
    case _ => None
  }

  def unapplyBind(arg: patterns.Pattern): Option[patterns.Bind.Data] = arg match {
    case Impl(Trees.Bind(name: Names.TermName, body), ctx) => Some(TermName(name), Pattern(body)(ctx))
    case _ => None
  }

  def unapplyUnapply(arg: patterns.Pattern): Option[patterns.Unapply.Data] = arg match {
    case Impl(Trees.UnApply(fun, implicits, patterns), ctx) => Some((Term(fun)(ctx), implicits.map(Term(_)(ctx)), patterns.map(Pattern(_)(ctx))))
    case _ => None
  }

  def unapplyAlternative(arg: patterns.Pattern): Option[patterns.Alternative.Data] = arg match {
    case Impl(Trees.Alternative(patterns), ctx) => Some(patterns.map(Pattern(_)(ctx)))
    case _ => None
  }

  def unapplyTypeTest(arg: patterns.Pattern): Option[patterns.TypeTest.Data] = arg match {
    case Impl(Trees.Typed(_, tpt), ctx) => Some(TypeTree(tpt)(ctx))
    case _ => None
  }

  def unapplyWildcard(arg: patterns.Pattern): Boolean = arg match {
    case Impl(Trees.Ident(name), _) => name == nme.WILDCARD
    case _ => false
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends patterns.Pattern with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case patterns.Value(v) => s"Value($v)"
        case patterns.Bind(name, body) => s"Bind($name, $body)"
        case patterns.Unapply(fun, implicits, patterns) => s"Unapply($fun, ${list(implicits)}, ${list(patterns)})"
        case patterns.Alternative(patterns) => s"Alternative(${list(patterns)})"
        case patterns.TypeTest(tpt) => s"TypeTest($tpt)"
        case patterns.Wildcard() => s"Wildcard()"
        case _ => s"Pattern {## $tree ##}"
      }
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}

