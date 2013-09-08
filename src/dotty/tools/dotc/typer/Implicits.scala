package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, untpd, tpd, TreeInfo}
import util.Positions._
import Contexts._
import Types._
import Flags._
import Mode.ImplicitsEnabled
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
import ErrorReporting._
import collection.mutable

/** Implicit resolution */
object Implicits {

  /** A common base class of contextual implicits and of-type implicits which
   *  represents as set of implicit references.
   */
  abstract class ImplicitRefs extends Compatibility {

    /** The implicit references */
    def refs: List[TermRef]

    /** Return those references in `refs` that are compatible with type `pt`. */
    protected def filterMatching(pt: Type)(implicit ctx: Context): List[TermRef] = {
      def result(implicit ctx: Context) =
        refs filter (ref => isCompatible(normalize(ref), pt))
      result(ctx.fresh.withNewTyperState) // create a defensive copy of ctx to avoid constraint pollution
    }

    /** No further implicit conversions can be applied when searching for implicits. */
    override def viewExists(tp: Type, pt: Type)(implicit ctx: Context) = false
  }

  /** The implicit references coming from the implicit scope of a type.
   *  @param tp              the type determining the implicit scope
   *  @param companionRefs   the companion objects in the implicit scope.
   */
  class OfTypeImplicits(tp: Type, val companionRefs: TermRefSet)(initctx: Context)
    extends ImplicitRefs {
    assert(initctx.typer != null)
    implicit val ctx: Context = initctx retractMode ImplicitsEnabled
    val refs: List[TermRef] = companionRefs.toList flatMap (_.implicitMembers)

    /** The implicit references that are eligible for expected type `tp` */
    lazy val eligible: List[TermRef] = filterMatching(tp)
  }

  /** The implicit references coming from the context.
   *  @param refs      the implicit references made visible by the current context.
   *                   Note: The name of the reference might be different from the name of its symbol.
   *                   In the case of a renaming import a => b, the name of the reference is the renamed
   *                   name, b, whereas the name of the symbol is the original name, a.
   *  @param outerCtx  the next outer context that makes visible further implicits
   */
  class ContextualImplicits(val refs: List[TermRefBySym], val outerCtx: Context)(initctx: Context) extends ImplicitRefs {
    implicit val ctx: Context =
      if (initctx == NoContext) initctx else initctx retractMode Mode.ImplicitsEnabled

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

    override def toString = {
      val own = s"(implicits: ${refs mkString ","})"
      if (outerCtx == NoContext) own else own + "\n " + outerCtx.implicits
    }
  }

  /** The result of an implicit search */
  abstract class SearchResult

  /** A successful search
   *  @param ref   The implicit reference that succeeeded
   *  @param tree  The typed tree that can needs to be inserted
   *  @param ctx   The context after the implicit search
   */
  case class SearchSuccess(tree: tpd.Tree)(val ref: TermRef, val tstate: TyperState) extends SearchResult

  /** A failed search */
  abstract class SearchFailure extends SearchResult {
    /** A note describing the failure in more detail - this
     *  is either empty or starts with a '\n'
     */
    def postscript(implicit ctx: Context): String = ""
  }

  /** A "no matching implicit found" failure */
  case object NoImplicitMatches extends SearchFailure

  /** A search failure that can show information about the cause */
  abstract class ExplainedSearchFailure extends SearchFailure {
    protected def pt: Type
    protected def argument: tpd.Tree
    protected def qualify(implicit ctx: Context) =
      if (argument.isEmpty) i"match type $pt"
      else i"convert from ${argument.tpe} to $pt"

    /** An explanation of the cause of the failure as a string */
    def explanation(implicit ctx: Context): String
  }

  /** An ambiguous implicits failure */
  class AmbiguousImplicits(alt1: TermRef, alt2: TermRef, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      i"both ${err.refStr(alt1)} and ${err.refStr(alt2)} $qualify"
    override def postscript(implicit ctx: Context) =
      "\nNote that implicit conversions cannot be applied because they are ambiguous;" +
      "\n " + explanation
  }

  class NonMatchingImplicit(ref: TermRef, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      i"${err.refStr(ref)} does not $qualify"
  }

  class ShadowedImplicit(ref: TermRef, shadowing: Type, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      i"${err.refStr(ref)} does $qualify but is shadowed by ${err.refStr(shadowing)}"
  }

  class FailedImplicit(failures: List[ExplainedSearchFailure], val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      if (failures.isEmpty) s"  No implicit candidates were found that $qualify"
      else "  " + (failures map (_.explanation) mkString "\n  ")
    override def postscript(implicit ctx: Context): String =
      "\nImplicit search failure summary:\n" + explanation
  }
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
  private object liftToClasses extends TypeMap {
    def apply(tp: Type) = tp match {
      case tp: TypeRef if tp.symbol.isAbstractOrAliasType =>
        val pre = tp.prefix
        def joinClass(tp: Type, cls: ClassSymbol) =
          AndType(tp, cls.symTypeRef.asSeenFrom(pre, cls.owner))
        (apply(tp.prefix) /: tp.classSymbols)(joinClass)
      case _ =>
        mapOver(tp)
    }
  }

  // todo: compute implicits directly, without going via companionRefs
  private def computeImplicitScope(tp: Type): OfTypeImplicits = {
    val comps = new TermRefSet
    tp match {
      case tp: NamedType =>
        val pre = tp.prefix
        comps ++= implicitScope(pre).companionRefs
        def addClassScope(cls: ClassSymbol): Unit = {
          def addRef(companion: TermRefBySym): Unit = {
            val comp1 @ TermRef(p, _) = companion.asSeenFrom(pre, companion.symbol.owner)
            comps += TermRef.withSym(p, comp1.symbol.asTerm).withDenot(comp1.denot)
          }
          def addParentScope(parent: TypeRef): Unit = {
            implicitScope(parent).companionRefs foreach addRef
            for (param <- parent.typeParams)
              comps ++= implicitScope(pre.member(param.name).info).companionRefs
          }
          val companion = cls.companionModule
          if (companion.exists) addRef(companion.symTermRef)
          cls.classParents foreach addParentScope
        }
        tp.classSymbols foreach addClassScope
      case _ =>
        for (part <- tp.namedParts)
          comps ++= implicitScope(part).companionRefs
    }
    new OfTypeImplicits(tp, comps)(ctx)
  }

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
    && (ctx.mode is Mode.ImplicitsEnabled)
    && (inferView(dummyTreeOfType(from), to).isInstanceOf[SearchSuccess]))

  /** Find an implicit conversion to apply to given tree `from` so that the
   *  result is compatible with type `to`.
   */
  def inferView(from: tpd.Tree, to: Type)(implicit ctx: Context): SearchResult =
    inferImplicit(to, from, from.pos)

  /** Find an implicit parameter or conversion.
   *  @param pt              The expected type of the parameter or conversion.
   *  @param argument        If an implicit conversion is searched, the argument to which
   *                         it should be applied, EmptyTree otherwise.
   *  @param pos             The position where errors should be reported.
   */
  def inferImplicit(pt: Type, argument: Tree, pos: Position)(implicit ctx: Context): SearchResult =
    ctx.traceIndented(s"search implicit $pt, arg = ${argument.show}", show = true) {
    val isearch =
      if (ctx.settings.explaintypes.value) new ExplainedImplicitSearch(pt, argument, pos)
      else new ImplicitSearch(pt, argument, pos)
    val result = isearch.bestImplicit
    result match {
      case success: SearchSuccess => success.tstate.commit()
      case _ =>
    }
    result
  }

  /** An implicit search; parameters as in `inferImplicit` */
  class ImplicitSearch(protected val pt: Type, protected val argument: Tree, pos: Position)(implicit ctx: Context) {

    protected def nonMatchingImplicit(ref: TermRef): SearchFailure = NoImplicitMatches
    protected def shadowedImplicit(ref: TermRef, shadowing: Type): SearchFailure = NoImplicitMatches
    protected def failedSearch: SearchFailure = NoImplicitMatches


    /** Search a list of eligible implicit references */
    def searchImplicits(eligible: List[TermRef], contextual: Boolean): SearchResult = {

      /** Try to typecheck an implicit reference */
      def typedImplicit(ref: TermRef)(implicit ctx: Context): SearchResult = {
        val id = Ident(ref).withPos(pos)
        val tree =
          if (argument.isEmpty) adapt(id, pt)
          else typedApply(id, ref, argument :: Nil, pt)
        lazy val shadowing = typed(untpd.Ident(ref.name), ref).tpe
        if (ctx.typerState.reporter.hasErrors) nonMatchingImplicit(ref)
        else if (contextual && !(shadowing =:= ref)) shadowedImplicit(ref, shadowing)
        else SearchSuccess(tree)(ref, ctx.typerState)
      }

      /** Given a list of implicit references, produce a list of all implicit search successes,
       *  where the first is supposed to be the best one.
       *  @param pending   The list of implicit references
       *  @param acc       An accumulator of successful matches found so far.
       */
      def rankImplicits(pending: List[TermRef], acc: List[SearchSuccess]): List[SearchSuccess] = pending match {
        case ref :: pending1 =>
          typedImplicit(ref)(ctx.fresh.withNewTyperState.retractMode(ImplicitsEnabled)) match {
            case fail: SearchFailure =>
              rankImplicits(pending1, acc)
            case best: SearchSuccess =>
              val newPending = pending filterNot (isAsGood(_, best.ref))
              rankImplicits(newPending, best :: acc)
          }
        case nil => acc
      }

      /** Convert a (possibly empty) list of search successes into a single search result */
      def condense(hits: List[SearchSuccess]): SearchResult = hits match {
        case best :: alts =>
          alts find (alt => isAsGood(alt.ref, best.ref)) match {
            case Some(alt) =>
              new AmbiguousImplicits(best.ref, alt.ref, pt, argument)
            case None =>
              ctx.runInfo.useCount(best.ref) += 1
              best
          }
        case Nil =>
          failedSearch
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

      condense(rankImplicits(sort(eligible), Nil))
    }

    /** The expected type where parameters and uninstantiated typevars
     *  are replaced by wildcard types
     */
    val wildPt: Type = {
      val result = (new WildApprox) apply pt
      if (argument.isEmpty) result
      else ViewProto(argument.tpe, result)
    }

    /** Find a unique best implicit reference */
    def bestImplicit: SearchResult = {
      searchImplicits(ctx.implicits.eligible(wildPt), contextual = true) match {
        case result: SearchSuccess => result
        case result: AmbiguousImplicits => result
        case result: SearchFailure =>
          searchImplicits(implicitScope(wildPt).eligible, contextual = false)
      }
    }

    def implicitScope(tp: Type): OfTypeImplicits = ctx.runInfo.implicitScope(tp)
  }

  final class ExplainedImplicitSearch(pt: Type, argument: Tree, pos: Position)(implicit ctx: Context)
  extends ImplicitSearch(pt, argument, pos) {
    private var myFailures = new mutable.ListBuffer[ExplainedSearchFailure]
    private def record(fail: ExplainedSearchFailure) = {
      myFailures += fail
      fail
    }
    def failures = myFailures.toList
    override def nonMatchingImplicit(ref: TermRef) =
      record(new NonMatchingImplicit(ref, pt, argument))
    override def shadowedImplicit(ref: TermRef, shadowing: Type): SearchFailure =
      record(new ShadowedImplicit(ref, shadowing, pt, argument))
    override def failedSearch: SearchFailure = {
      //println(s"wildPt = $wildPt")
      //println(s"implicit scope = ${implicitScope(wildPt).companionRefs}")
      new FailedImplicit(failures, pt, argument)
    }
  }
}

/** A set of term references where equality is =:= */
class TermRefSet(implicit ctx: Context) extends mutable.Traversable[TermRefBySym] {
  private val elems = new mutable.LinkedHashMap[TermSymbol, List[Type]]

  def += (ref: TermRefBySym): Unit = {
    val pre = ref.prefix
    val sym = ref.symbol.asTerm
    elems get sym match {
      case Some(prefixes) =>
        if (!(prefixes exists (_ =:= pre))) elems(sym) = pre :: prefixes
      case None =>
        elems(sym) = pre :: Nil
    }
  }

  def ++= (refs: TraversableOnce[TermRefBySym]): Unit =
    refs foreach +=

  override def foreach[U](f: TermRefBySym => U): Unit =
    for (sym <- elems.keysIterator)
      for (pre <- elems(sym))
        f(TermRef.withSym(pre, sym))
}