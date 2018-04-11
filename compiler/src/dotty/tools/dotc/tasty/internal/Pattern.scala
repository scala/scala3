package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.StdNames._

import scala.tasty.pattern

object Pattern {

  def apply(tree: tpd.Tree)(implicit ctx: Context): pattern.Pattern = Impl(tree, ctx)

  object Value {
    def unapply(arg: pattern.Pattern): Option[scala.tasty.term.Term] = arg match {
      case Impl(lit: tpd.Literal, ctx) => Some(Term(lit)(ctx))
      case _ => None
    }
  }

  object Bind {
    def unapply(arg: pattern.Pattern): Option[(scala.tasty.TermName, pattern.Pattern)] = arg match {
      case Impl(Trees.Bind(name: Names.TermName, body), ctx) => Some(TermName(name), Pattern(body)(ctx))
      case _ => None
    }
  }

  object Unapply {
    def unapply(arg: pattern.Pattern): Option[(scala.tasty.term.Term, List[scala.tasty.term.Term], List[pattern.Pattern])] = arg match {
      case Impl(Trees.UnApply(fun, implicits, patterns), ctx) => Some((Term(fun)(ctx), implicits.map(Term(_)(ctx)), patterns.map(Pattern(_)(ctx))))
      case _ => None
    }
  }

  object Alternative {
    def unapply(arg: pattern.Pattern): Option[List[pattern.Pattern]] = arg match {
      case Impl(Trees.Alternative(patterns), ctx) => Some(patterns.map(Pattern(_)(ctx)))
      case _ => None
    }
  }

  object TypeTest {
    def unapply(arg: pattern.Pattern): Option[scala.tasty.typetree.TypeTree] = arg match {
      case Impl(Trees.Typed(_, tpt), ctx) => Some(TypeTree(tpt)(ctx))
      case _ => None
    }
  }

  object Wildcard {
    def unapply(arg: pattern.Pattern): Boolean = arg match {
      case Impl(Trees.Ident(name), _) => name == nme.WILDCARD
      case _ => false
    }
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends pattern.Pattern with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def toString: String = this match {
      case Value(v) => s"Value($v)"
      case Bind(name, body) => s"Bind($name, $body)"
      case Unapply(fun, implicits, patterns) => s"Unapply($fun, $implicits, $patterns)"
      case Alternative(patterns) => s"Alternative($patterns)"
      case TypeTest(tpt) => s"TypeTest($tpt)"
      case Wildcard() => s"Wildcard()"
      case _ => s"Pattern {## $tree ##}"
    }

    private def list(xs: List[scala.tasty.term.Term]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}

