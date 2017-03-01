package dotty.tools.dotc
package transform

import TreeTransforms._
import core.Contexts._
import core.Symbols._
import core.Types._
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.{Flags, TypeApplications}
import dotty.tools.dotc.typer.Applications
import dotty.tools.dotc.util.Positions
import typer.ErrorReporting._
import ast.Trees._
import Applications._
import TypeApplications._
import SymUtils._
import core.NameOps._
import core.Mode
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags

class PatternTypeFactorization extends PatternFactorization {
  import dotty.tools.dotc.ast.tpd._

  def phaseName: String = "patternTypeFactorization"

  override def runsAfter = Set(classOf[TryCatchPatterns])

  protected def shouldSwap(caseDef1: CaseDef, caseDef2: CaseDef)(implicit ctx: Context): Boolean = false && {
    // fixme: this is wrong in case of primitives at least. run/matchbytes.scala demostrates this
    val tpe1 = caseDef1.pat.tpe.widen
    val tpe2 = caseDef2.pat.tpe.widen
    tpe1.exists && tpe2.exists && !(tpe1 <:< tpe2) && !(tpe2 <:< tpe1) && tpe1.uniqId < tpe2.uniqId
  }

  protected def factorized(cases: List[CaseDef])(implicit ctx: Context, info: TransformerInfo): (List[List[CaseDef]], List[CaseDef]) = {
    val reordered = reorderedCases(cases)
    val preFactored = factorCases(reordered)
    val (factoredTypes, fallbacks) = preFactored.span(hasCaseWithoutGuard)
    if (fallbacks.nonEmpty) {
      (factoredTypes :+ fallbacks.head, fallbacks.tail.flatten)
    } else {
      (factoredTypes, Nil)
    }
  }

  protected def asInnerMatchIfNeeded(sel: Symbol, caseDefs: List[CaseDef], fallbackOpt: Option[Tree])(implicit ctx: Context, info: TransformerInfo): CaseDef = {
    assert(caseDefs.nonEmpty)
    val fallbackCase = fallbackOpt.map(CaseDef(Underscore(caseDefs.head.pat.tpe.widen), EmptyTree, _))
    asInnerMatch(sel, caseDefs ++ fallbackCase)
  }

  protected def factorCases(cases: List[CaseDef])(implicit ctx: Context, info: TransformerInfo): List[List[CaseDef]] = {
    def loop(remaining: List[CaseDef], groups: List[List[CaseDef]]): List[List[CaseDef]] = {
      remaining match {
        case c0 :: tail =>
          val tpe = c0.pat.tpe.widen
          val (span, rest) = tail.span(_.pat.tpe <:< tpe)
          loop(rest, (c0 :: span) :: groups)

        case Nil => groups.reverse
      }
    }
    loop(cases, Nil)
  }

  protected def asInnerMatch(sel: Symbol, cases: List[CaseDef])(
    implicit ctx: Context, info: TransformerInfo): CaseDef = {
    assert(cases.nonEmpty)
    val tpe = cases.head.pat.tpe.widen.orElse(sel.info.widen)
    val selName = ctx.freshName("fact").toTermName
    val factorizedSelector =
      ctx.newSymbol(ctx.owner, selName, Flags.Synthetic | Flags.Case, tpe)
    val selector = Ident(factorizedSelector.termRef)
    val pattern = Bind(factorizedSelector, Typed(Underscore(factorizedSelector.info), TypeTree(factorizedSelector.info)))
    val innerMatch = transformFollowing(Match(selector, cases))
    CaseDef(pattern, EmptyTree, innerMatch)
  }
}
