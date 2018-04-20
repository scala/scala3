package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

import scala.tasty.trees
import scala.tasty.types

object Pattern {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.Pattern = new Impl(tree)

  def unapplyValue(arg: Impl): Option[trees.Value.Data] = {
    implicit val ctx: Context = arg.ctx
    arg.tree match {
      case lit: tpd.Literal => Some(Term(lit))
      case ident: tpd.Ident => Some(Term(ident))
      case _ => None
    }
  }

  def unapplyBind(arg: Impl): Option[trees.Bind.Data] = arg.tree match {
    case Trees.Bind(name: Names.TermName, body) =>
      implicit val ctx: Context = arg.ctx
      Some(TermName(name), Pattern(body))
    case _ => None
  }

  def unapplyUnapply(arg: Impl): Option[trees.Unapply.Data] = arg.tree match {
    case Trees.UnApply(fun, implicits, patterns) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(fun), implicits.map(Term(_)), patterns.map(Pattern(_))))
    case _ => None
  }

  def unapplyAlternative(arg: Impl): Option[trees.Alternative.Data] = arg.tree match {
    case Trees.Alternative(patterns) =>
      implicit val ctx: Context = arg.ctx
      Some(patterns.map(Pattern(_)))
    case _ => None
  }

  def unapplyTypeTest(arg: Impl): Option[trees.TypeTest.Data] = arg.tree match {
    case Trees.Typed(_, tpt) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeTree(tpt))
    case _ => None
  }

  private[tasty] class Impl(val tree: tpd.Tree)(implicit val ctx: Context) extends trees.Pattern with Positioned {

    def tpe: types.Type = Type(tree.tpe)

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

