package dottyBench.tools.dotc
package transform

import core.Symbols._
import core.StdNames._
import ast.Trees._
import core.Types._
import core.NameKinds.ExceptionBinderName
import dottyBench.tools.dotc.core.Flags
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.transform.MegaPhase.MiniPhase
import dottyBench.tools.dotc.util.Spans.Span

/** Compiles the cases that can not be handled by primitive catch cases as a common pattern match.
 *
 *  The following code:
 *    ```
 *    try { <code> }
 *    catch {
 *      <tryCases> // Cases that can be handled by catch
 *      <patternMatchCases> // Cases starting with first one that can't be handled by catch
 *    }
 *    ```
 *  will become:
 *    ```
 *    try { <code> }
 *    catch {
 *      <tryCases>
 *      case e => e match {
 *        <patternMatchCases>
 *      }
 *    }
 *    ```
 *
 *  Cases that are not supported include:
 *   - Applies and unapplies
 *   - Idents
 *   - Alternatives
 *   - `case _: T =>` where `T` is not `Throwable`
 *
 */
class TryCatchPatterns extends MiniPhase {
  import dottyBench.tools.dotc.ast.tpd._

  def phaseName: String = "tryCatchPatterns"

  override def runsAfter: Set[String] = Set(ElimRepeated.name)

  override def checkPostCondition(tree: Tree)(using Ctx, CState): Unit = tree match {
    case Try(_, cases, _) =>
      cases.foreach {
        case CaseDef(Typed(_, _), guard, _) => assert(guard.isEmpty, "Try case should not contain a guard.")
        case CaseDef(Bind(_, _), guard, _) => assert(guard.isEmpty, "Try case should not contain a guard.")
        case c =>
          assert(isDefaultCase(c), "Pattern in Try should be Bind, Typed or default case.")
      }
    case _ =>
  }

  override def transformTry(tree: Try)(using Ctx, CState): Tree = {
    val (tryCases, patternMatchCases) = tree.cases.span(isCatchCase)
    val fallbackCase = mkFallbackPatterMatchCase(patternMatchCases, tree.span)
    cpy.Try(tree)(cases = tryCases ++ fallbackCase)
  }

  /** Is this pattern node a catch-all or type-test pattern? */
  private def isCatchCase(cdef: CaseDef)(using Ctx, CState): Boolean = cdef match {
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _)          => isSimpleThrowable(tpt.tpe)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) => isSimpleThrowable(tpt.tpe)
    case _                                                               => isDefaultCase(cdef)
  }

  private def isSimpleThrowable(tp: Type)(using Ctx, CState): Boolean = tp.stripAnnots match {
    case tp @ TypeRef(pre, _) =>
      (pre == NoPrefix || pre.widen.typeSymbol.isStatic) && // Does not require outer class check
      !tp.symbol.is(Flags.Trait) && // Traits not supported by JVM
      tp.derivesFrom(defn.ThrowableClass)
    case tp: AppliedType =>
      isSimpleThrowable(tp.tycon)
    case _ =>
      false
  }

  private def mkFallbackPatterMatchCase(patternMatchCases: List[CaseDef], span: Span)(
      using Ctx, CState): Option[CaseDef] =
    if (patternMatchCases.isEmpty) None
    else {
      val exName = ExceptionBinderName.fresh()
      val fallbackSelector =
        newSymbol(ctx.owner, exName, Flags.Synthetic | Flags.Case, defn.ThrowableType, coord = span)
      val sel = Ident(fallbackSelector.termRef).withSpan(span)
      val rethrow = CaseDef(EmptyTree, EmptyTree, Throw(ref(fallbackSelector)))
      Some(CaseDef(
          Bind(fallbackSelector, Underscore(fallbackSelector.info).withSpan(span)),
          EmptyTree,
          transformFollowing(Match(sel, patternMatchCases ::: rethrow :: Nil)))
      )
    }
}

