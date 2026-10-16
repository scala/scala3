package dotty.tools.dotc
package transform

import core.Contexts.*
import core.Flags
import core.NameKinds.ExceptionBinderName
import core.Symbols.*
import core.StdNames.*
import core.Types.*
import reporting.UnreasonableCatch
import util.chaining.*
import util.Spans.Span
import util.SrcPos
import MegaPhase.MiniPhase

import scala.annotation.tailrec

/** Compiles the cases that can not be handled by primitive catch cases as a common pattern match.
 *  Also emits warnings for cases that are bad ideas, such as catching Throwable or Error.
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
  import ast.tpd.*
  import TryCatchPatterns.SimpleMultiCatch

  override def phaseName: String = TryCatchPatterns.name

  override def description: String = TryCatchPatterns.description

  override def runsAfter: Set[String] = Set(ElimRepeated.name)

  override def checkPostCondition(tree: Tree)(using Context): Unit = tree match {
    case Try(_, cases, _) =>
      cases.foreach {
        case CaseDef(Typed(_, _), guard, _) => assert(guard.isEmpty, "Try case should not contain a guard.")
        case CaseDef(Bind(_, _), guard, _) => assert(guard.isEmpty, "Try case should not contain a guard.")
        case c =>
          assert(isDefaultCase(c), "Pattern in Try should be Bind, Typed or default case.")
      }
    case _ =>
  }

  override def transformTry(tree: Try)(using Context): Tree = {
    val (tryCases, patternMatchCases) = tree.cases.span(isCatchCase)
    val fallbackCase = mkFallbackPatterMatchCase(patternMatchCases, tree.span)
    cpy.Try(tree)(cases = transformTryCases(tryCases) ++ fallbackCase)
  }

  private def transformTryCases(tryCases: List[CaseDef])(using Context) = tryCases.mapConserve {
    case cdef @ CaseDef(Typed(_, tpt @ DisjointUnion(types)), _, _) =>
      cdef.withAttachment(SimpleMultiCatch, types)
    case cdef @ CaseDef(Bind(_, Typed(_, tpt @ DisjointUnion(types))), _, _) =>
      cdef.withAttachment(SimpleMultiCatch, types)
    case cdef => cdef
  }

  /** Is this pattern node a catch-all or type-test pattern? */
  private def isCatchCase(cdef: CaseDef)(using Context): Boolean = cdef match
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), guard, _) =>
      warnIfUnreasonableCatch(tpt)
      guard == EmptyTree && isSimpleThrowable(tpt)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), guard, _) =>
      warnIfUnreasonableCatch(tpt)
      guard == EmptyTree && isSimpleThrowable(tpt)
    case _ =>
      isDefaultCase(cdef).tap(if _ then warnForUnreasonableCatch(cdef))

  private def isSimpleThrowable(tpt: Tree)(using Context): Boolean = tpt match
    case DisjointUnion(types) => types.forall(isSimpleThrowable)
    case _ => isSimpleThrowable(tpt.tpe)

  @tailrec
  private def isSimpleThrowable(tp: Type)(using Context): Boolean = tp.strippedDealias match {
    case tp @ TypeRef(pre, _) =>
      (pre == NoPrefix || pre.typeSymbol.isStatic) && // Does not require outer class check
      !tp.symbol.is(Flags.Trait) && // Traits not supported by JVM
      tp.derivesFrom(defn.ThrowableClass)
    case tp: AppliedType =>
      isSimpleThrowable(tp.tycon)
    case _ =>
      false
  }

  private def mkFallbackPatterMatchCase(patternMatchCases: List[CaseDef], span: Span)(
      implicit ctx: Context): Option[CaseDef] =
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

  private def warnIfUnreasonableCatch(tpt: Tree)(using Context): Unit =
    if ctx.settings.Whas.unreasonableCatch && tpt.tpe <:< defn.ErrorType || tpt.tpe =:= defn.ThrowableType then
      report.warning(UnreasonableCatch(Some(tpt.tpe)), tpt)

  private def warnForUnreasonableCatch(pos: SrcPos)(using Context): Unit =
    if ctx.settings.Whas.unreasonableCatch then
      report.warning(UnreasonableCatch(None), pos)
}

object TryCatchPatterns:
  val name: String = "tryCatchPatterns"
  val description: String = "compile cases in try/catch"
  import util.Property
  val SimpleMultiCatch: Property.StickyKey[List[Type]] = Property.StickyKey()
