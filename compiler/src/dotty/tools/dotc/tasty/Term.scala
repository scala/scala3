package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

object Term {

  def apply(tree: tpd.Tree)(implicit ctx: Context): scala.tasty.Term = Impl(tree, ctx)

  object Ident {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.TermName)] = term match {
      case Impl(id@Trees.Ident(name: Names.TermName), _) if id.isTerm => Some(TermName(name))
      case _ => None
    }
  }

  object Select {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, scala.tasty.PossiblySignedName)] = term match {
      case Impl(id@Trees.Select(qual, name: Names.TermName), ctx) if id.isTerm => Some(Term(qual)(ctx), TermName(name))
      case _ => None
    }
  }

  object Literal {
    def unapply(term: scala.tasty.TopLevelStatement): Option[scala.tasty.Constant] = term match {
      case Impl(Trees.Literal(const), _) => Some(Constant(const))
      case _ => None
    }
  }

  object This {
    def unapply(term: scala.tasty.TopLevelStatement): Option[Option[scala.tasty.Id]] = term match {
      case Impl(Trees.This(qual), ctx) => Some(if (qual.isEmpty) None else Some(Id(qual)(ctx)))
      case _ => None
    }
  }

  object New {
    def unapply(term: scala.tasty.TopLevelStatement): Option[scala.tasty.TypeTree] = term match {
      case Impl(Trees.New(tpt), ctx) => Some(TypeTree(tpt)(ctx))
      case _ => None
    }
  }

  object NamedArg {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.TermName, scala.tasty.Term)] = term match {
      case Impl(Trees.NamedArg(name: Names.TermName, arg), ctx) => Some(TermName(name), Term(arg)(ctx))
      case _ => None
    }
  }

  object Apply {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, List[scala.tasty.Term])] = term match {
      case Impl(Trees.Apply(fn, args), ctx) => Some((Term(fn)(ctx), args.map(arg => Term(arg)(ctx))))
      case _ => None
    }
  }

  object TypeApply {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, List[scala.tasty.Term])] = term match {
      case Impl(Trees.TypeApply(fn, args), ctx) => Some((Term(fn)(ctx), args.map(arg => Term(arg)(ctx))))
      case _ => None
    }
  }
  object Super {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, Option[scala.tasty.Id])] = term match {
      case Impl(Trees.Super(qual, mixin), ctx) => Some((Term(qual)(ctx), if (mixin.isEmpty) None else Some(Id(mixin)(ctx))))
      case _ => None
    }
  }

  object Typed {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, scala.tasty.TypeTree)] = term match {
      case Impl(Trees.Typed(expr, tpt), ctx) => Some((Term(expr)(ctx), TypeTree(tpt)(ctx)))
      case _ => None
    }
  }

  object Assign {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, scala.tasty.Term)] = term match {
      case Impl(Trees.Assign(lhs, rhs), ctx) => Some((Term(lhs)(ctx), Term(rhs)(ctx)))
      case _ => None
    }
  }

  object Block {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(List[scala.tasty.Statement], scala.tasty.Term)] = term match {
      case Impl(Trees.Block(stats, expr), ctx) => Some((stats.map(stat => Statement(stat)(ctx)), Term(expr)(ctx)))
      case _ => None
    }
  }

//  case Inlined(call: Term, bindings: List[Definition], expr: Term)

  object Lambda {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, Option[scala.tasty.TypeTree])] = term match {
      case Impl(Trees.Closure(_, meth, tpt), ctx) => Some((Term(meth)(ctx), if (tpt.isEmpty) None else Some(TypeTree(tpt)(ctx))))
      case _ => None
    }
  }

  object If {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, scala.tasty.Term, scala.tasty.Term)] = term match {
      case Impl(Trees.If(cond, thenp, elsep), ctx) => Some((Term(cond)(ctx), Term(thenp)(ctx), Term(elsep)(ctx)))
      case _ => None
    }
  }

  object Match {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, List[scala.tasty.CaseDef])] = term match {
      case Impl(Trees.Match(selector, cases), ctx) => Some((Term(selector)(ctx), cases.map(c => CaseDef(c)(ctx))))
      case _ => None
    }
  }

  object Try {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, List[scala.tasty.CaseDef], Option[scala.tasty.Term])] = term match {
      case Impl(Trees.Try(body, catches, finalizer), ctx) => Some((Term(body)(ctx), catches.map(c => CaseDef(c)(ctx)), if (finalizer.isEmpty) None else Some(Term(finalizer)(ctx))))
      case _ => None
    }
  }

  object Return {
    def unapply(term: scala.tasty.TopLevelStatement): Option[scala.tasty.Term] = term match {
      case Impl(Trees.Return(expr, from), ctx) => Some(Term(expr)(ctx)) // TODO use `from` or remove it
      case _ => None
    }
  }

  object Repeated {
    def unapply(term: scala.tasty.TopLevelStatement): Option[List[scala.tasty.Term]] = term match {
      case Impl(Trees.SeqLiteral(args, elemtpt), ctx) => Some(args.map(arg => Term(arg)(ctx))) // TODO use `elemtpt`?
      case _ => None
    }
  }

//  case SelectOuter(from: Term, levels: Int, target: Type) // can be generated by inlining

  private case class Impl(tree: tpd.Tree, ctx: Context) extends scala.tasty.Term with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def toString: String = this match {
      case Ident(name) => s"Ident($name)"
      case Select(qual, name) => s"Select($qual, $name)"
      case Literal(const) => s"Literal($const)"
      case New(tpt) => s"New($tpt)"
      case NamedArg(name, arg) => s"NamedArg($name, $arg)"
      case Apply(fn, args) => s"Apply($fn, ${list(args)})"
      case TypeApply(fn, args) => s"TypeApply($fn, ${list(args)})"
      case Super(qual, mixin) => s"Super($qual, $mixin)"
      case Typed(expr, tpt) => s"Typed($expr, $tpt)"
      case Assign(lhs, rhs) => s"Assign($lhs, $rhs)"
      case Block(stats, expr) => s"Block(${list(stats)}, $expr)"
      case Lambda(meth, tpt) => s"Lambda($meth, $tpt)"
      case If(cond, thenp, elsep) => s"If($cond, $thenp, $elsep)"
      case Match(selector, cases) => s"Match($selector, ${list(cases)})"
      case Try(body, catches, finalizer) => s"Try($body, ${list(catches)}, $finalizer)"
      case Return(expr) => s"Return($expr)"
      case Repeated(args) => s"Repeated($args)"
      case _ => s"Term{## $tree ##}"
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}
