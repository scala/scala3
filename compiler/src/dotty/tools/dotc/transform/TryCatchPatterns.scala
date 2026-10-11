package dotty.tools.dotc
package transform

import core.Symbols.*
import core.StdNames.*
import core.Types.*
import core.NameKinds.ExceptionBinderName
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.UnreasonableCatch
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SrcPos

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
  import dotty.tools.dotc.ast.tpd.*

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
    val (tryCases, patternMatchCases) = tree.cases.partitionMap(isCatchCase)
    val fallbackCase = mkFallbackPatterMatchCase(patternMatchCases, tree.span)
    cpy.Try(tree)(cases = tryCases ++ fallbackCase)
  }

  /** Is this pattern node a catch-all or type-test pattern? */
  // private def isCatchCase(cdef: CaseDef)(using Context): Boolean = cdef match {
  //   case CaseDef(Typed(Ident(nme.WILDCARD), tpt @ DisjointUnion(types)), guard, _) =>
  //     guard == EmptyTree && types.forall(isSimpleThrowable)
  //   case CaseDef(Typed(Ident(nme.WILDCARD), tpt), guard, _) =>
  //     warnIfUnreasonableCatch(tpt)
  //     guard == EmptyTree && isSimpleThrowable(tpt.tpe)
  //   case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt @ DisjointUnion(types))), guard, _) =>
  //     warnIfUnreasonableCatch(tpt)
  //     guard == EmptyTree && types.forall(isSimpleThrowable)
  //   case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), guard, _) =>
  //     warnIfUnreasonableCatch(tpt)
  //     guard == EmptyTree && isSimpleThrowable(tpt.tpe)
  //   case _ =>
  //     if isDefaultCase(cdef) then
  //       warnForUnreasonableCatch(cdef)
  //       true
  //     else
  //       false
  // }
  import TryCatchPatterns.SimpleMultiCatch

  /** Is this pattern node a catch-all or type-test pattern? */
  private def isCatchCase(cdef: CaseDef)(using Context): Either[CaseDef, CaseDef] = cdef match {
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt @ DisjointUnion(types)), guard, _) =>
      if(guard == EmptyTree && types.forall(isSimpleThrowable))
        Left(cdef.withAttachment(SimpleMultiCatch, types))
      else Right(cdef)
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), guard, _) =>
      warnIfUnreasonableCatch(tpt)
      if(guard == EmptyTree && isSimpleThrowable(tpt.tpe))
        Left(cdef)
      else Right(cdef)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt @ DisjointUnion(types))), guard, _) =>
      warnIfUnreasonableCatch(tpt)
      if(guard == EmptyTree && types.forall(isSimpleThrowable))
        Left(cdef.withAttachment(SimpleMultiCatch, types))
      else Right(cdef)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), guard, _) =>
      warnIfUnreasonableCatch(tpt)
      if(guard == EmptyTree && isSimpleThrowable(tpt.tpe))
        Left(cdef)
      else Right(cdef)
    case _ =>
      if isDefaultCase(cdef) then
        warnForUnreasonableCatch(cdef)
        Left(cdef)
      else
        Right(cdef)
  }

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

  private def collectAltTypes(t: Type)(using Context): List[Type] = t match {
    case OrType(tp1,tp2) =>
      (collectAltTypes(tp1) ::: collectAltTypes(tp2))
    case _ => List(t)
  }
  private def isDisjointThrowable(tp:Type)(using Context): Boolean =
    collectAltTypes(tp)
     .combinations(2)
     .flatMap(_.permutations)
     .map {
        case head :: next :: Nil =>
          !(head <:< next)
        case _ => false
      }.forall(_ == true)

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
  import core.Types.Type
  val SimpleMultiCatch: Property.StickyKey[List[Type]] = Property.StickyKey()
