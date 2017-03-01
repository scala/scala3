package dotty.tools.dotc
package transform

import TreeTransforms._
import core.Contexts._
import core.Constants._
import dotty.tools.dotc.ast.tpd
import ast.Trees._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.ast.Trees._
import TreeTransforms._

class PatternConstantsFactorization extends PatternFactorization {
  import dotty.tools.dotc.ast.tpd._

  def phaseName: String = "patternConstantsFactorization"

  //override def runsAfter = Set(classOf[PatternTypeFactorization])

  override def transformTry(tree: Try)(implicit ctx: Context, info: TransformerInfo): Tree = tree

  protected def shouldSwap(caseDef1: CaseDef, caseDef2: CaseDef)(implicit ctx: Context): Boolean = {
    (caseDef1.pat, caseDef2.pat) match {
      case (Literal(const1), Literal(const2)) =>
        if (const1 == const2) false
        else const1.stringValue > const2.stringValue
      case _ => false
    }
  }

  protected def isOnConstant(caseDef: CaseDef): Boolean = caseDef match {
    case CaseDef(Literal(Constant(_)), _, _) => true
    case _                                   => false
  }

  protected def factorized(cases: List[CaseDef])(implicit ctx: Context, info: TransformerInfo): (List[List[CaseDef]], List[CaseDef]) = {
    val reordered = reorderedCases(cases)
    val preFactored = factorCases(reordered)
    val (factoredConstants, fallbacks) =
      preFactored.span(cases => isOnConstant(cases.head))
    if (factoredConstants.nonEmpty) {
      (factoredConstants, fallbacks.flatten)
    } else {
      val (fact, fallbacks1) = fallbacks.span(cases => !isOnConstant(cases.head))
      if (fallbacks1.nonEmpty) (fact, fallbacks1.flatten)
      else (Nil, fallbacks.flatten)
    }
  }

  protected def asInnerMatchIfNeeded(sel: Symbol, caseDefs: List[CaseDef], fallbackOpt: Option[Tree])(implicit ctx: Context, info: TransformerInfo): CaseDef = {
    assert(caseDefs.nonEmpty)
    caseDefs.head match {
      case caseDef @ CaseDef(Literal(_), EmptyTree, _) if caseDefs.size == 1 => caseDef
      case CaseDef(lit @ Literal(_), _, _) =>
        val fallbackCase = fallbackOpt.map(CaseDef(lit, EmptyTree, _))
        asInnerMatchOnConstant(lit, caseDefs ++ fallbackCase)
      case caseDef =>
        val fallbackCase = fallbackOpt.map(CaseDef(Underscore(caseDef.pat.tpe.widen), EmptyTree, _))
        asInnerMatch(caseDefs ++ fallbackCase)
    }
  }

  protected def factorCases(cases: List[CaseDef])(implicit ctx: Context, info: TransformerInfo): List[List[CaseDef]] = {
    def loop(remaining: List[CaseDef], groups: List[List[CaseDef]]): List[List[CaseDef]] = {
      remaining match {
        case CaseDef(lit @ Literal(_), _, _) :: _ =>
          val (span, rest) = remaining.span {
            case CaseDef(Literal(Constant(value)), _, _) => value == lit.const.value
            case _ => false
          }
          loop(rest, span :: groups)

        case _ :: _ =>
          val (span, rest) = remaining.span {
            case CaseDef(Literal(_), _, _) => false
            case _ => true
          }
          loop(rest, span :: groups)

        case Nil => groups.reverse
      }
    }
    loop(cases, Nil)
  }

  protected def asInnerMatchOnConstant(lit: Literal, cases: List[CaseDef])(
      implicit ctx: Context, info: TransformerInfo): CaseDef = {
    val innerMatch = transformFollowing(Match(lit, cases))
    CaseDef(lit, EmptyTree, innerMatch)
  }

  protected def asInnerMatch(cases: List[CaseDef])(
    implicit ctx: Context, info: TransformerInfo): CaseDef = {
    assert(cases.nonEmpty)
    val tpe = cases.head.pat.tpe.widen
    val selName = ctx.freshName("fact").toTermName
    val factorizedSelector =
      ctx.newSymbol(ctx.owner, selName, Flags.Synthetic | Flags.Case, tpe)
    val selector = Ident(factorizedSelector.termRef)
    val pattern = Bind(factorizedSelector, Underscore(factorizedSelector.info))
    val innerMatch = transformFollowing(Match(selector, cases))
    CaseDef(pattern, EmptyTree, innerMatch)
  }

}
