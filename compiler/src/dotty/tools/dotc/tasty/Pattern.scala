package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.sourcePos
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.StdNames._


object Pattern {

  def apply(tree: Tree)(implicit ctx: Context): scala.tasty.Pattern = Impl(tree, ctx)

  object Value {
    def unapply(pattern: scala.tasty.Pattern): Option[scala.tasty.Term] = pattern match {
      case Impl(lit@Trees.Literal(_), ctx) => Some(Term(lit)(ctx))
      case _ => None
    }
  }

  object Bind {
    def unapply(pattern: scala.tasty.Pattern): Option[(scala.tasty.TermName, scala.tasty.Pattern)] = pattern match {
      case Impl(Trees.Bind(name: Names.TermName, body), ctx) => Some(TermName(name), Pattern(body)(ctx))
      case _ => None
    }
  }

  object Unapply {
    def unapply(pattern: scala.tasty.Pattern): Option[(scala.tasty.Term, List[scala.tasty.Term], List[scala.tasty.Pattern])] = pattern match {
      case Impl(Trees.UnApply(fun, implicits, patterns), ctx) => Some((Term(fun)(ctx), implicits.map(Term(_)(ctx)), patterns.map(Pattern(_)(ctx))))
      case _ => None
    }
  }

  object Alternative {
    def unapply(pattern: scala.tasty.Pattern): Option[List[scala.tasty.Pattern]] = pattern match {
      case Impl(Trees.Alternative(patterns), ctx) => Some(patterns.map(Pattern(_)(ctx)))
      case _ => None
    }
  }

  object TypeTest {
    def unapply(pattern: scala.tasty.Pattern): Option[scala.tasty.TypeTree] = pattern match {
      case Impl(Trees.Typed(_, tpt), ctx) => Some(TypeTree(tpt)(ctx))
      case _ => None
    }
  }

  object Wildcard {
    def unapply(pattern: scala.tasty.Pattern): Boolean = pattern match {
      case Impl(Trees.Ident(name), _) => name == nme.WILDCARD
      case _ => false
    }
  }

  private case class Impl(tree: Tree, ctx: Context) extends scala.tasty.Pattern {

    implicit def ctx_ : Context = ctx

    def pos: scala.tasty.Position = new dotty.tools.dotc.tasty.Position(tree.pos)

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def toString: String = this match {
      case Bind(name, body) => s"Bind($name, $body)"
      case Unapply(fun, implicits, patterns) => s"Unapply($fun, $implicits, $patterns)"
      case Alternative(patterns) => s"Alternative($patterns)"
      case TypeTest(tpt) => s"TypeTest($tpt)"
      case Wildcard() => s"Wildcard()"
      case _ => s"Pattern {## $tree ##}"
    }

    private def list(xs: List[scala.tasty.Term]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}

