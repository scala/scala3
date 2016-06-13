package dotty.tools.dotc
package transform

import TreeTransforms._
import core.Contexts._
import core.Symbols._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags

/** Factorize type checks in pattern matches.
 *
 *  {{{
 *  expr match {
 *    case List(1, 2) =>
 *    case List(1, 2, 3) =>
 *    case 1 =>
 *    case 2 =>
 *    case 3 =>
 *    case _: List[Int] if guard =>
 *    case Some(x) if pred(x) =>
 *    case Some(x) =>
 *    case None =>
 *  }
 *  }}}
 *
 *  will be converted to
 *
 *  {{{
 *  val selector = expr
 *  def fallback() = { // labeled definition
 *    selector match {
 *      case _: List[Int] if guard =>
 *      case factor: Some =>
 *        // no type check as the type is known to be List[Int]
 *        factor match {
 *          case Some(x) if pred(x) =>
 *          case Some(x)            =>
 *          case _                  => fallback()
 *        }
 *      case None =>
 *    }
 *  }
 *  selector match { // this will become a switch
 *    case factor: List[Int] =>
 *      // no type check as the type is known to be List[Int]
 *      factor match {
 *        case List(1, 2)    =>
 *        case List(1, 2, 3) =>
 *        case _             => fallback()
 *      }
 *    case factor: Int =>
 *      factor match { // this will become a switch on int
 *        case 1 =>
 *        case 2 =>
 *        case 3 =>
 *        case _ => fallback()
 *      }
 *    case _ => fallback()
 *  }
 *  }}}
 */
class PatternTypeFactorization extends PatternFactorization {
  import dotty.tools.dotc.ast.tpd._

  def phaseName: String = "patternTypeFactorization"

  override def runsAfter = Set(classOf[TryCatchPatterns])

  protected def shouldSwap(caseDef1: CaseDef, caseDef2: CaseDef)(implicit ctx: Context): Boolean = {
    val tpe1 = caseDef1.pat.tpe.widen
    val tpe2 = caseDef2.pat.tpe.widen
    !(tpe1 <:< tpe2) && !(tpe2 <:< tpe1) && tpe1.uniqId < tpe2.uniqId
  }

  protected def factorized(cases: List[CaseDef])(implicit ctx: Context, info: TransformerInfo): (List[List[CaseDef]], List[CaseDef]) = {
    val reordered = reorderedCases(cases)
    val preFactored = factorCases(reordered)

    def findFallback(remaining: List[List[CaseDef]],
        taken: List[List[CaseDef]]): (List[List[CaseDef]], List[CaseDef]) = {
      remaining match {
        case remainingHead :: remainingTail =>
          val tpe = remainingHead.head.pat.tpe
          if (hasCaseWithoutGuard(remainingHead) && !remainingTail.exists(_.head.pat.tpe <:< tpe))
            findFallback(remainingTail, remainingHead :: taken)
          else
            ((remainingHead :: taken).reverse, remainingTail.flatten)
        case Nil =>
          (taken.reverse, Nil)
      }
    }
    findFallback(preFactored, Nil)
  }

  protected def asInnerMatchIfNeeded(caseDefs: List[CaseDef], fallbackOpt: Option[Tree])(implicit ctx: Context, info: TransformerInfo): CaseDef = {
    assert(caseDefs.nonEmpty)
    val fallbackCase = fallbackOpt.map(CaseDef(Underscore(caseDefs.head.pat.tpe.widen), EmptyTree, _))
    asInnerMatch(caseDefs ++ fallbackCase)
  }

  protected def factorCases(cases: List[CaseDef])(implicit ctx: Context, info: TransformerInfo): List[List[CaseDef]] = {
    def loop(remaining: List[CaseDef], groups: List[List[CaseDef]]): List[List[CaseDef]] = {
      remaining match {
        case c0 :: _ =>
          val tpe = c0.pat.tpe.widen
          val (span, rest) = remaining.span(_.pat.tpe.widen =:= tpe)
          loop(rest, span :: groups)

        case Nil => groups.reverse
      }
    }
    loop(cases, Nil)
  }

  protected def asInnerMatch(cases: List[CaseDef])(
    implicit ctx: Context, info: TransformerInfo): CaseDef = {
    assert(cases.nonEmpty)
    val tpe = cases.head.pat.tpe.widen
    val selName = ctx.freshName("fact").toTermName
    val factorizedSelector =
      ctx.newSymbol(ctx.owner, selName, Flags.Synthetic | Flags.Case, tpe)
    val selector = Ident(factorizedSelector.termRef)
    val pattern = Bind(factorizedSelector, Typed(Underscore(factorizedSelector.info), TypeTree(factorizedSelector.info)))
    val innerMatch = transformFollowing(Match(selector, cases))
    CaseDef(pattern, EmptyTree, innerMatch)
  }
}
