package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, untpd, tpd, TreeInfo}
import util.Positions._
import Contexts._
import Types._
import Flags._
import Denotations._
import NameOps._
import SymDenotations._
import Symbols._
import Types._
import Decorators._
import Names._
import StdNames._
import Constants._
import Inferencing._
import Applications._
import collection.mutable

/** Implicit resolution */
object Implicits {

  /** A common base class of contextual implicits and of-type implicits which
   *  represents as set of implicit references.
   */
  abstract class ImplicitRefs extends Compatibility with Normalizing {

    /** The implicit references */
    def refs: Set[TermRef]

    /** Return those references in `refs` that are compatible with type `pt`. */
    protected def filterMatching(pt: Type)(implicit ctx: Context) =
      refs.toList filter (ref => isCompatible(normalize(ref), pt))

    /** No further implicit conversions can be applied when searching for implicits. */
    override def viewExists(tp: Type, pt: Type)(implicit ctx: Context) = false
  }

  /** The implicit references coming from the implicit scope of a type.
   *  @param tp              the type determining the implicit scope
   *  @param companionRefs   the companion objects in the implicit scope.
   */
  class OfTypeImplicits(tp: Type, val companionRefs: Set[TermRef])(implicit ctx: Context)
  extends ImplicitRefs{
    val refs: Set[TermRef] = companionRefs flatMap (_.implicitMembers)

    /** The implicit references that are eligible for expected type `tp` */
    lazy val eligible: List[TermRef] = filterMatching(tp)
  }

  /** The implicit references coming from the context.
   *  @param refs      the implicit references made visible by the current context
   *  @param outerCtx  the next outer context that makes visible further implicits
   */
  class ContextualImplicits(val refs: Set[TermRef], val outerCtx: Context)(implicit val ctx: Context) extends ImplicitRefs {
    private val eligibleCache = new mutable.HashMap[Type, List[TermRef]]

    /** The implicit references that are eligible for type `tp`. */
    def eligible(tp: Type): List[TermRef] =
      if (tp.hash == NotCached) computeEligible(tp)
      else eligibleCache.getOrElseUpdate(tp, computeEligible(tp))

    private def computeEligible(tp: Type): List[TermRef] = {
      val ownEligible = filterMatching(tp)
      if (outerCtx == NoContext) ownEligible
      else ownEligible ::: {
        val shadowed = (ownEligible map (_.name)).toSet
        outerCtx.implicits.eligible(tp).filter(ref => !(shadowed contains ref.name))
      }
    }
  }

  /** The result of an implicit search */
  abstract class SearchResult

  /** A successful search
   *  @param ref   The implicit reference that succeeeded
   *  @param tree  The typed tree that can needs to be inserted
   *  @param ctx   The context after the implicit search
   */
  case class SearchSuccess(ref: TermRef, tree: tpd.Tree, ctx: Context) extends SearchResult

  /** A failed search */
  abstract class SearchFailure extends SearchResult

  /** An ambiguous implicits failure */
  case class AmbiguousImplicits(alt1: TermRef, alt2: TermRef) extends SearchFailure

  /** A "no matching implicit found" failure */
  case object NoImplicitMatches extends SearchFailure
}

import Implicits._

/** Info relating to implicits that is kept for one run */
trait ImplicitRunInfo { self: RunInfo =>

  private val implicitScopeCache = mutable.HashMap[Type, OfTypeImplicits]()

  /** Replace every typeref that does not refer to a class by a conjunction of class types
   *  that has the same implicit scope as the original typeref. The motivation for applying
   *  this map is that it reduces the total number of types for which we need to
   *  compute and cache the implicit scope; all variations wrt type parameters or
   *  abstract types are eliminated.
   */
  private val liftToClasses = new TypeMap {
    def apply(tp: Type) = tp match {
      case tp: TypeRef if !tp.symbol.isClass =>
        val pre = tp.prefix
        def joinClass(tp: Type, cls: ClassSymbol) =
          AndType(tp, cls.symTypeRef.asSeenFrom(pre, cls.owner))
        (apply(tp.prefix) /: tp.classSymbols)(joinClass)
      case _ =>
        mapOver(tp)
    }
  }

  private def computeImplicitScope(tp: Type): OfTypeImplicits =
    new OfTypeImplicits(tp,
      tp match {
        case tp: NamedType =>
          val pre = tp.prefix
          def addClassScope(comps: Set[TermRef], cls: ClassSymbol): Set[TermRef] = {
            def addRef(comps: Set[TermRef], comp: TermRef): Set[TermRef] =
              comps + comp.asSeenFrom(pre, comp.symbol.owner).asInstanceOf[TermRef]
            def addInheritedScope(comps: Set[TermRef], parent: TypeRef): Set[TermRef] = {
              val baseTp = cls.thisType.baseType(parent.symbol)
              (comps /: implicitScope(baseTp).companionRefs)(addRef)
            }
            val companion = cls.companionModule
            val comps1 = if (companion.exists) addRef(comps, companion.symTermRef) else comps
            (comps1 /: cls.classParents)(addInheritedScope)
          }
          (implicitScope(pre).companionRefs /: tp.classSymbols)(addClassScope)
        case _ =>
          tp.namedParts.flatMap(implicitScope(_).companionRefs)
      })

  /** The implicit scope of a type
   *  @param isLifted    Type `tp` is the result of a `liftToClasses` application
   */
  def implicitScope(tp: Type, isLifted: Boolean = false): OfTypeImplicits =
    if (tp.hash == NotCached) computeImplicitScope(tp)
    else implicitScopeCache.getOrElseUpdate(tp, {
      val liftedTp = if (isLifted) tp else liftToClasses(tp)
      if (liftedTp ne tp) implicitScope(liftedTp, isLifted = true)
      else computeImplicitScope(tp)
    })

  /** A map that counts the number of times an implicit ref was picked */
  val useCount = new mutable.HashMap[TermRef, Int] {
    override def default(key: TermRef) = 0
  }
}

/** The implicit resolution part of type checking */
trait Implicits { self: Typer =>

  import tpd._

  override def viewExists(from: Type, to: Type)(implicit ctx: Context): Boolean = (
       !from.isError
    && !to.isError
    && ctx.implicitsEnabled
    && inferView(dummyTreeOfType(from), to) != EmptyTree
    )

  /** Find an implicit conversion to apply to given tree `from` so that the
   *  result is compatible with type `to`.
   */
  def inferView(from: tpd.Tree, to: Type)(implicit ctx: Context): Tree =
    inferImplicit(to, from, from.pos, reportAmbiguous = false)

  /** Find an implicit parameter or conversion.
   *  @param pt              The expected type of the parameter or conversion.
   *  @param argument        If an implicit conversion is searched, the argument to which
   *                         it should be applied, EmptyTree otherwise.
   *  @param pos             The position where errors should be reported.
   *  @param reportAmbiguous Should ambiguity errors be reported? False when called from `viewExists`.
   */
  def inferImplicit(pt: Type, argument: Tree, pos: Position, reportAmbiguous: Boolean = true)(implicit ctx: Context): Tree = {
    new ImplicitSearch(pt, argument, pos).bestImplicit match {
      case SearchSuccess(_, tree, newCtx) =>
        ctx.typerState.copyFrom(newCtx.typerState)
        tree
      case NoImplicitMatches =>
        EmptyTree
      case AmbiguousImplicits(alt1, alt2) =>
        if (reportAmbiguous) {
          val qualify =
            if (argument.isEmpty) s"match type $pt"
            else s"can convert from ${argument.tpe} to $pt"
          ctx.error(s"ambiguous implicits; both $alt1 and $alt2 $qualify")
        }
        EmptyTree
    }
  }

  /** An implicit search; parameters as in `inferImplicit` */
  class ImplicitSearch(pt: Type, argument: Tree, pos: Position)(implicit ctx: Context) {

    /** Try to typecheck an implicit reference */
    def typedImplicit(ref: TermRef)(implicit ctx: Context): SearchResult = {
      val id = Ident(ref).withPos(pos)
      val tree =
        if (argument.isEmpty) adapt(id, Mode.Expr, pt)
        else typedApply(id, ref, argument :: Nil, pt)
      if (tree.tpe.isError) NoImplicitMatches  // todo: replace by checking if local errors were reported in ctx.
      else SearchSuccess(ref, tree, ctx)
    }

    /** Given a list of implicit references, produce a list of all implicit search successes,
     *  where the first is supposed to be the best one.
     *  @param pending   The list of implicit references
     *  @param acc       An accumulator of successful matches found so far.
     */
    private def rankImplicits(pending: List[TermRef], acc: List[SearchSuccess]): List[SearchSuccess] = pending match {
      case ref :: pending1 =>
        typedImplicit(ref)(ctx.fresh.withNewTyperState.silent) match {
          case fail: SearchFailure =>
            rankImplicits(pending1, acc)
          case best @ SearchSuccess(bestRef, _, _) =>
            val newPending = pending filterNot (isAsGood(_, bestRef))
            rankImplicits(newPending, best :: acc)
        }
      case nil => acc
    }

    /** Convert a (possibly empty) list of search successes into a single search result */
    def condense(hits: List[SearchSuccess]): SearchResult = hits match {
      case best :: alts =>
        alts find (alt => isAsGood(alt.ref, best.ref)) match {
          case Some(alt) =>
            AmbiguousImplicits(best.ref, alt.ref)
          case None =>
            ctx.runInfo.useCount(best.ref) += 1
            best
        }
      case Nil =>
        NoImplicitMatches
    }

    /** Sort list of implicit references according to their popularity
     *  (# of times each was picked in current run).
     */
    def sort(eligible: List[TermRef]) = eligible match {
      case Nil => eligible
      case e1 :: Nil => eligible
      case e1 :: e2 :: Nil =>
        if (ctx.runInfo.useCount(e1) < ctx.runInfo.useCount(e2)) e2 :: e1 :: Nil
        else eligible
      case _ => eligible.sortBy(-ctx.runInfo.useCount(_))
    }

    /** Search a list of eligible implicit references */
    def searchImplicits(eligible: List[TermRef]): SearchResult = {
      condense(rankImplicits(sort(eligible), Nil))
    }

    /** The expected type where parameters and uninstantiated typevars
     *  are replaced by wildcard types
     */
    val wildPt: Type = (new WildApprox) apply pt

    /** Find a unique best implicit reference */
    def bestImplicit: SearchResult = {
       searchImplicits(ctx.implicits.eligible(wildPt)) match {
         case result: SearchSuccess => result
         case result: AmbiguousImplicits => result
         case NoImplicitMatches => searchImplicits(implicitScope(wildPt).eligible)
       }
    }

    def implicitScope(tp: Type): OfTypeImplicits = ctx.runInfo.implicitScope(tp)
  }

}