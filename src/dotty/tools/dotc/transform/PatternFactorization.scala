package dotty.tools.dotc.transform

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.ast.Trees._
import TreeTransforms._
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

trait PatternFactorization extends MiniPhaseTransform {
  import dotty.tools.dotc.ast.tpd._

  protected def asInnerMatchIfNeeded(caseDefs: List[CaseDef], fallbackOpt: Option[Tree])(implicit ctx: Context, info: TransformerInfo): CaseDef
  protected def factorized(cases: List[CaseDef])(implicit ctx: Context, info: TransformerInfo): (List[List[CaseDef]], List[CaseDef])
  protected def shouldSwap(case1: CaseDef, case2: CaseDef)(implicit ctx: Context): Boolean

  protected def hasCaseWithoutGuard(cases: List[CaseDef]): Boolean = {
    cases.exists {
      case CaseDef(_, EmptyTree, _) => true
      case _                        => false
    }
  }

  override def transformMatch(tree: Match)(implicit ctx: Context, info: TransformerInfo): Tree = {
    //println(s">>> ${this.getClass.getName}.transformMatch  " + tree.selector.tpe.show + " " + tree.tpe.show)
    //println(tree.show)
    //tree.cases.foreach(println)
    //println()
    val (factoredCases, fallbackCases) = factorized(tree.cases)
    if (factoredCases.nonEmpty) {
      val selectorSym =
        ctx.newSymbol(ctx.owner, ctx.freshName("selector").toTermName, Flags.Synthetic, tree.selector.tpe)
      val selectorVal = ValDef(selectorSym, tree.selector)
      val selector = Ident(selectorSym.termRef)

      val fallbackDefDefOpt = {
        if (fallbackCases.nonEmpty) {
          val fallbackMatch = transformMatch(Match(selector, fallbackCases))
          val fallbackName = ctx.freshName("fallback").toTermName
          val fallbackSym =
            ctx.newSymbol(ctx.owner, fallbackName, Flags.Synthetic | Flags.Label, MethodType(Nil, Nil)(x => fallbackMatch.tpe))
          Some(DefDef(fallbackSym, fallbackMatch))
        } else {
          None
        }
      }
      val fallbackOpt = fallbackDefDefOpt.map { fallbackDefDef =>
        Apply(Ident(fallbackDefDef.symbol.termRef), Nil)
      }

      val newFactoredCases = factoredCases.map(asInnerMatchIfNeeded(_, fallbackOpt))

      val fallbackCaseOpt = fallbackOpt.map { fallback =>
        CaseDef(Underscore(fallback.symbol.info), EmptyTree, fallback)
      }

      Block(
          List(selectorVal) ++ fallbackDefDefOpt,
          transformFollowing(cpy.Match(tree)(selector, newFactoredCases ++ fallbackCaseOpt))
      )
    } else {
      transformFollowing(tree)
    }
  }

  protected def reorderedCases(cases: List[CaseDef])(implicit ctx: Context, info: TransformerInfo): List[CaseDef] = {
    val casesArray = cases.toArray
    var swapped = false
    do {
      swapped = false
      for (i <- 1 until casesArray.length) {
        val tmp1 = casesArray(i - 1)
        val tmp2 = casesArray(i)
        if (shouldSwap(tmp1, tmp2)) {
          swapped = true
          casesArray(i - 1) = tmp2
          casesArray(i) = tmp1
        }
      }
    } while (swapped)

    casesArray.toList
  }
}
