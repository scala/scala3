package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, untpd, tpd, TreeInfo}
import util.Positions._
import util.Stats.{track, record, monitored}
import printing.{Showable, Printer}
import printing.Texts._
import Contexts._
import Types._
import Flags._
import TypeErasure.{erasure, hasStableErasure}
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
import Applications._
import ProtoTypes._
import ErrorReporting._
import reporting.diagnostic.MessageContainer
import Inferencing.fullyDefinedType
import Trees._
import Hashable._
import util.Property
import config.Config
import config.Printers.{implicits, implicitsDetailed, typr}
import collection.mutable

/** Implicit resolution */
object Implicits {

  /** A reference to an implicit value to be made visible on the next nested call to
   *  inferImplicitArg with a by-name expected type.
   */
  val DelayedImplicit = new Property.Key[TermRef]

  /** An eligible implicit candidate, consisting of an implicit reference and a nesting level */
  case class Candidate(ref: TermRef, level: Int)

  /** A common base class of contextual implicits and of-type implicits which
   *  represents a set of implicit references.
   */
  abstract class ImplicitRefs(initctx: Context) {
    implicit val ctx: Context =
      if (initctx == NoContext) initctx else initctx retractMode Mode.ImplicitsEnabled

    /** The nesting level of this context. Non-zero only in ContextialImplicits */
    def level: Int = 0

    /** The implicit references */
    def refs: List[TermRef]

    /** Return those references in `refs` that are compatible with type `pt`. */
    protected def filterMatching(pt: Type)(implicit ctx: Context): List[Candidate] = track("filterMatching") {

      def refMatches(ref: TermRef)(implicit ctx: Context) = /*ctx.traceIndented(i"refMatches $ref $pt")*/ {

        def discardForView(tpw: Type, argType: Type): Boolean = tpw match {
          case mt: MethodType =>
            mt.isImplicit ||
            mt.paramInfos.length != 1 ||
            !(argType relaxed_<:< mt.paramInfos.head)(ctx.fresh.setExploreTyperState)
          case poly: PolyType =>
            // We do not need to call ProtoTypes#constrained on `poly` because
            // `refMatches` is always called with mode TypevarsMissContext enabled.
            poly.resultType match {
              case mt: MethodType =>
                mt.isImplicit ||
                mt.paramInfos.length != 1 ||
                !(argType relaxed_<:< wildApprox(mt.paramInfos.head, null, Set.empty)(ctx.fresh.setExploreTyperState))
              case rtp =>
                discardForView(wildApprox(rtp, null, Set.empty), argType)
            }
          case tpw: TermRef =>
            false // can't discard overloaded refs
          case tpw =>
            // Only direct instances of Function1 and direct or indirect instances of <:< are eligible as views.
            // However, Predef.$conforms is not eligible, because it is a no-op.
            //
            // In principle, it would be cleanest if only implicit methods qualified
            // as implicit conversions. We could achieve that by having standard conversions like
            // this in Predef:
            //
            //    implicit def convertIfConforms[A, B](x: A)(implicit ev: A <:< B): B = ev(a)
            //    implicit def convertIfConverter[A, B](x: A)(implicit ev: ImplicitConverter[A, B]): B = ev(a)
            //
            // (Once `<:<` inherits from `ImplicitConverter` we only need the 2nd one.)
            // But clauses like this currently slow down implicit search a lot, because
            // they are eligible for all pairs of types, and therefore are tried too often.
            // We emulate instead these conversions directly in the search.
            // The reason for leaving out `Predef_conforms` is that we know it adds
            // nothing since it only relates subtype with supertype.
            //
            // We keep the old behavior under -language:Scala2.
            val isFunctionInS2 = ctx.scala2Mode && tpw.derivesFrom(defn.FunctionClass(1))
            val isImplicitConverter = tpw.derivesFrom(defn.Predef_ImplicitConverter)
            val isConforms =
              tpw.derivesFrom(defn.Predef_Conforms) && ref.symbol != defn.Predef_conforms
            !(isFunctionInS2 || isImplicitConverter || isConforms)
        }

        def discardForValueType(tpw: Type): Boolean = tpw.stripPoly match {
          case tpw: MethodType => !tpw.isImplicit
          case _ => false
        }

        def discard = pt match {
          case pt: ViewProto => discardForView(ref.widen, pt.argType)
          case _: ValueTypeOrProto => !defn.isFunctionType(pt) && discardForValueType(ref.widen)
          case _ => false
        }

        (ref.symbol isAccessibleFrom ref.prefix) && {
          if (discard) {
            record("discarded eligible")
            false
          }
          else NoViewsAllowed.isCompatible(normalize(ref, pt), pt)
        }
      }

      if (refs.isEmpty) Nil
      else refs.filter(refMatches(_)(ctx.fresh.addMode(Mode.TypevarsMissContext).setExploreTyperState)) // create a defensive copy of ctx to avoid constraint pollution
               .map(Candidate(_, level))
    }
  }

  /** The implicit references coming from the implicit scope of a type.
   *  @param tp              the type determining the implicit scope
   *  @param companionRefs   the companion objects in the implicit scope.
   */
  class OfTypeImplicits(tp: Type, val companionRefs: TermRefSet)(initctx: Context) extends ImplicitRefs(initctx) {
    assert(initctx.typer != null)
    lazy val refs: List[TermRef] = {
      val buf = new mutable.ListBuffer[TermRef]
      for (companion <- companionRefs) buf ++= companion.implicitMembers
      buf.toList
    }

    /** The candidates that are eligible for expected type `tp` */
    lazy val eligible: List[Candidate] =
      /*>|>*/ track("eligible in tpe") /*<|<*/ {
        /*>|>*/ ctx.traceIndented(i"eligible($tp), companions = ${companionRefs.toList}%, %", implicitsDetailed, show = true) /*<|<*/ {
          if (refs.nonEmpty && monitored) record(s"check eligible refs in tpe", refs.length)
          filterMatching(tp)
        }
      }

    override def toString =
      i"OfTypeImplicits($tp), companions = ${companionRefs.toList}%, %; refs = $refs%, %."
  }

  /** The implicit references coming from the context.
   *  @param refs      the implicit references made visible by the current context.
   *                   Note: The name of the reference might be different from the name of its symbol.
   *                   In the case of a renaming import a => b, the name of the reference is the renamed
   *                   name, b, whereas the name of the symbol is the original name, a.
   *  @param outerCtx  the next outer context that makes visible further implicits
   */
  class ContextualImplicits(val refs: List[TermRef], val outerImplicits: ContextualImplicits)(initctx: Context) extends ImplicitRefs(initctx) {
    private val eligibleCache = new mutable.AnyRefMap[Type, List[Candidate]]

    /** The level increases if current context has a different owner or scope than
     *  the context of the next-outer ImplicitRefs. This is however disabled under
     *  Scala2 mode, since we do not want to change the implicit disambiguation then.
     */
    override val level: Int =
      if (outerImplicits == null) 1
      else if (ctx.scala2Mode ||
               (ctx.owner eq outerImplicits.ctx.owner) &&
               (ctx.scope eq outerImplicits.ctx.scope)) outerImplicits.level
      else outerImplicits.level + 1

    /** Is this the outermost implicits? This is the case if it either the implicits
     *  of NoContext, or the last one before it.
     */
    private def isOuterMost = {
      val finalImplicits = NoContext.implicits
      (this eq finalImplicits) || (outerImplicits eq finalImplicits)
    }

    /** The implicit references that are eligible for type `tp`. */
    def eligible(tp: Type): List[Candidate] = /*>|>*/ track(s"eligible in ctx") /*<|<*/ {
      if (tp.hash == NotCached) computeEligible(tp)
      else eligibleCache get tp match {
        case Some(eligibles) =>
          def elided(ci: ContextualImplicits): Int = {
            val n = ci.refs.length
            if (ci.isOuterMost) n
            else n + elided(ci.outerImplicits)
          }
          if (monitored) record(s"elided eligible refs", elided(this))
          eligibles
        case None =>
          if (ctx eq NoContext) Nil
          else {
            val savedEphemeral = ctx.typerState.ephemeral
            ctx.typerState.ephemeral = false
            try {
              val result = computeEligible(tp)
              if (ctx.typerState.ephemeral) record("ephemeral cache miss: eligible")
              else eligibleCache(tp) = result
              result
            } finally ctx.typerState.ephemeral |= savedEphemeral
          }
      }
    }

    private def computeEligible(tp: Type): List[Candidate] = /*>|>*/ ctx.traceIndented(i"computeEligible $tp in $refs%, %", implicitsDetailed) /*<|<*/ {
      if (monitored) record(s"check eligible refs in ctx", refs.length)
      val ownEligible = filterMatching(tp)
      if (isOuterMost) ownEligible
      else ownEligible ::: {
        val shadowed = ownEligible.map(_.ref.name).toSet
        outerImplicits.eligible(tp).filterNot(cand => shadowed.contains(cand.ref.name))
      }
    }

    override def toString = {
      val own = i"(implicits: $refs%, %)"
      if (isOuterMost) own else own + "\n " + outerImplicits
    }

    /** This context, or a copy, ensuring root import from symbol `root`
     *  is not present in outer implicits.
     */
    def exclude(root: Symbol): ContextualImplicits =
      if (this == NoContext.implicits) this
      else {
        val outerExcluded = outerImplicits exclude root
        if (ctx.importInfo.site.termSymbol == root) outerExcluded
        else if (outerExcluded eq outerImplicits) this
        else new ContextualImplicits(refs, outerExcluded)(ctx)
      }
  }

  /** The result of an implicit search */
  sealed abstract class SearchResult extends Showable {
    def toText(printer: Printer): Text = printer.toText(this)
  }

  /** A successful search
   *  @param ref   The implicit reference that succeeded
   *  @param tree  The typed tree that needs to be inserted
   *  @param ctx   The context after the implicit search
   */
  case class SearchSuccess(tree: tpd.Tree, ref: TermRef, level: Int, tstate: TyperState) extends SearchResult with Showable {
    override def toString = s"SearchSuccess($tree, $ref, $level)"
  }

  /** A failed search */
  abstract class SearchFailure extends SearchResult {
    /** A note describing the failure in more detail - this
     *  is either empty or starts with a '\n'
     */
    def postscript(implicit ctx: Context): String = ""
  }

  /** A "no matching implicit found" failure */
  case object NoImplicitMatches extends SearchFailure

  case object DivergingImplicit extends SearchFailure

  /** A search failure that can show information about the cause */
  abstract class ExplainedSearchFailure extends SearchFailure {
    protected def pt: Type
    protected def argument: tpd.Tree
    protected def qualify(implicit ctx: Context) =
      if (argument.isEmpty) em"match type $pt"
      else em"convert from ${argument.tpe} to $pt"

    /** An explanation of the cause of the failure as a string */
    def explanation(implicit ctx: Context): String
  }

  /** An ambiguous implicits failure */
  class AmbiguousImplicits(val alt1: TermRef, val alt2: TermRef, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      em"both ${err.refStr(alt1)} and ${err.refStr(alt2)} $qualify"
    override def postscript(implicit ctx: Context) =
      "\nNote that implicit conversions cannot be applied because they are ambiguous;" +
      "\n " + explanation
  }

  class NonMatchingImplicit(ref: TermRef,
                            val pt: Type,
                            val argument: tpd.Tree,
                            trail: List[MessageContainer]) extends ExplainedSearchFailure {
    private val separator = "\n**** because ****\n"

    /** Replace repeated parts beginning with `separator` by ... */
    private def elideRepeated(str: String): String = {
      val startIdx = str.indexOfSlice(separator)
      val nextIdx = str.indexOfSlice(separator, startIdx + separator.length)
      if (nextIdx < 0) str
      else {
        val prefix = str.take(startIdx)
        val first = str.slice(startIdx, nextIdx)
        var rest = str.drop(nextIdx)
        if (rest.startsWith(first)) {
          rest = rest.drop(first.length)
          val dots = "\n\n     ...\n"
          if (!rest.startsWith(dots)) rest = dots ++ rest
        }
        prefix ++ first ++ rest
      }
    }

    def explanation(implicit ctx: Context): String = {
      val headMsg = em"${err.refStr(ref)} does not $qualify"
      val trailMsg = trail.map(mc => i"$separator  ${mc.message}").mkString
      elideRepeated(headMsg ++ trailMsg)
    }
  }

  class ShadowedImplicit(ref: TermRef, shadowing: Type, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      em"${err.refStr(ref)} does $qualify but is shadowed by ${err.refStr(shadowing)}"
  }

  class DivergingImplicit(ref: TermRef, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      em"${err.refStr(ref)} produces a diverging implicit search when trying to $qualify"
  }

  class FailedImplicit(failures: List[ExplainedSearchFailure], val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      if (failures.isEmpty) s"  No implicit candidates were found that $qualify"
      else "  " + (failures map (_.explanation) mkString "\n  ")
    override def postscript(implicit ctx: Context): String =
      i"""
         |Implicit search failure summary:
         |$explanation"""
  }
}

import Implicits._

/** Info relating to implicits that is kept for one run */
trait ImplicitRunInfo { self: RunInfo =>

  private val implicitScopeCache = mutable.AnyRefMap[Type, OfTypeImplicits]()

  /** The implicit scope of a type `tp`
   *  @param liftingCtx   A context to be used when computing the class symbols of
   *                      a type. Types may contain type variables with their instances
   *                      recorded in the current context. To find out the instance of
   *                      a type variable, we need the current context, the current
   *                      runinfo context does not do.
   */
  def implicitScope(rootTp: Type, liftingCtx: Context): OfTypeImplicits = {

    val seen: mutable.Set[Type] = mutable.Set()
    val incomplete: mutable.Set[Type] = mutable.Set()

    /** Replace every typeref that does not refer to a class by a conjunction of class types
     *  that has the same implicit scope as the original typeref. The motivation for applying
     *  this map is that it reduces the total number of types for which we need to
     *  compute and cache the implicit scope; all variations wrt type parameters or
     *  abstract types are eliminated.
     */
    object liftToClasses extends TypeMap {
      override implicit protected val ctx: Context = liftingCtx
      override def stopAtStatic = true
      def apply(tp: Type) = tp match {
        case tp: TypeRef if tp.symbol.isAbstractOrAliasType =>
          val pre = tp.prefix
          def joinClass(tp: Type, cls: ClassSymbol) =
            AndType.make(tp, cls.typeRef.asSeenFrom(pre, cls.owner), AndType.unchecked)
          val lead = if (tp.prefix eq NoPrefix) defn.AnyType else apply(tp.prefix)
          (lead /: tp.classSymbols)(joinClass)
        case tp: TypeVar =>
          apply(tp.underlying)
        case tp: HKApply =>
          def applyArg(arg: Type) = arg match {
            case TypeBounds(lo, hi) => AndType.make(lo, hi, AndType.unchecked)
            case _: WildcardType => defn.AnyType
            case _ => arg
          }
          (apply(tp.tycon) /: tp.args)((tc, arg) => AndType.make(tc, applyArg(arg), AndType.unchecked))
        case tp: TypeLambda =>
          apply(tp.resType)
        case _ =>
          mapOver(tp)
      }
    }

    // todo: compute implicits directly, without going via companionRefs?
    def collectCompanions(tp: Type): TermRefSet = track("computeImplicitScope") {
      ctx.traceIndented(i"collectCompanions($tp)", implicits) {

        def iscopeRefs(t: Type): TermRefSet = implicitScopeCache.get(t) match {
          case Some(is) =>
            is.companionRefs
          case None =>
            if (seen contains t) {
              incomplete += tp  // all references to rootTo will be accounted for in `seen` so we return `EmptySet`.
              EmptyTermRefSet   // on the other hand, the refs of `tp` are now not accurate, so `tp` is marked incomplete.
            } else {
              seen += t
              val is = iscope(t)
              if (!implicitScopeCache.contains(t)) incomplete += tp
              is.companionRefs
            }
        }

        val comps = new TermRefSet
        tp match {
          case tp: NamedType =>
            val pre = tp.prefix
            comps ++= iscopeRefs(pre)
            def addClassScope(cls: ClassSymbol): Unit = {
              def addRef(companion: TermRef): Unit = {
                val compSym = companion.symbol
                if (compSym is Package)
                  addRef(TermRef.withSig(companion, nme.PACKAGE, Signature.NotAMethod))
                else if (compSym.exists)
                  comps += companion.asSeenFrom(pre, compSym.owner).asInstanceOf[TermRef]
              }
              def addParentScope(parent: TypeRef): Unit = {
                iscopeRefs(parent) foreach addRef
                for (param <- parent.typeParamSymbols)
                  comps ++= iscopeRefs(tp.member(param.name).info)
              }
              val companion = cls.companionModule
              if (companion.exists) addRef(companion.valRef)
              cls.classParents foreach addParentScope
            }
            tp.classSymbols(liftingCtx) foreach addClassScope
          case _ =>
            // We exclude lower bounds to conform to SLS 7.2:
            // "The parts of a type T are: [...] if T is an abstract type, the parts of its upper bound"
            for (part <- tp.namedPartsWith(_.isType, excludeLowerBounds = true))
              comps ++= iscopeRefs(part)
        }
        comps
      }
    }

   /** The implicit scope of type `tp`
     *  @param isLifted    Type `tp` is the result of a `liftToClasses` application
     */
    def iscope(tp: Type, isLifted: Boolean = false): OfTypeImplicits = {
      val canCache = Config.cacheImplicitScopes && tp.hash != NotCached
      def computeIScope() = {
        val savedEphemeral = ctx.typerState.ephemeral
        ctx.typerState.ephemeral = false
        try {
          val liftedTp = if (isLifted) tp else liftToClasses(tp)
          val refs =
            if (liftedTp ne tp)
              iscope(liftedTp, isLifted = true).companionRefs
            else
              collectCompanions(tp)
          val result = new OfTypeImplicits(tp, refs)(ctx)
          if (ctx.typerState.ephemeral)
            record("ephemeral cache miss: implicitScope")
          else if (canCache &&
                   ((tp eq rootTp) ||          // first type traversed is always cached
                    !incomplete.contains(tp))) // other types are cached if they are not incomplete
            implicitScopeCache(tp) = result
          result
        }
        finally ctx.typerState.ephemeral |= savedEphemeral
      }
      if (canCache) implicitScopeCache.getOrElse(tp, computeIScope())
      else computeIScope()
    }

    iscope(rootTp)
  }

  /** A map that counts the number of times an implicit ref was picked */
  val useCount = new mutable.HashMap[TermRef, Int] {
    override def default(key: TermRef) = 0
  }

  def clear() = implicitScopeCache.clear()
}

/** The implicit resolution part of type checking */
trait Implicits { self: Typer =>

  import tpd._

  override def viewExists(from: Type, to: Type)(implicit ctx: Context): Boolean = (
       !from.isError
    && !to.isError
    && !ctx.isAfterTyper
    && (ctx.mode is Mode.ImplicitsEnabled)
    && from.isValueType
    && (  from.isValueSubType(to)
       || inferView(dummyTreeOfType(from), to)
            (ctx.fresh.addMode(Mode.ImplicitExploration).setExploreTyperState)
            .isInstanceOf[SearchSuccess]
       )
    )

  /** Find an implicit conversion to apply to given tree `from` so that the
   *  result is compatible with type `to`.
   */
  def inferView(from: Tree, to: Type)(implicit ctx: Context): SearchResult = track("inferView") {
    if (   (to isRef defn.AnyClass)
        || (to isRef defn.ObjectClass)
        || (to isRef defn.UnitClass)
        || (from.tpe isRef defn.NothingClass)
        || (from.tpe isRef defn.NullClass)
        || !(ctx.mode is Mode.ImplicitsEnabled)
        || (from.tpe eq NoPrefix)) NoImplicitMatches
    else {
      def adjust(to: Type) = to.stripTypeVar.widenExpr match {
        case SelectionProto(name, memberProto, compat, true) =>
          SelectionProto(name, memberProto, compat, privateOK = false)
        case tp => tp
      }
      try inferImplicit(adjust(to), from, from.pos)
      catch {
        case ex: AssertionError =>
          implicits.println(s"view $from ==> $to")
          implicits.println(ctx.typerState.constraint.show)
          implicits.println(TypeComparer.explained(implicit ctx => from.tpe <:< to))
          throw ex
      }
    }
  }

  /** Find an implicit argument for parameter `formal`.
   *  @param error  An error handler that gets an error message parameter
   *                which is itself parameterized by another string,
   *                indicating where the implicit parameter is needed
   */
  def inferImplicitArg(formal: Type, error: (String => String) => Unit, pos: Position)(implicit ctx: Context): Tree = {

    /** If `formal` is of the form ClassTag[T], where `T` is a class type,
     *  synthesize a class tag for `T`.
   	 */
    def synthesizedClassTag(formal: Type, pos: Position)(implicit ctx: Context): Tree = {
      if (formal.isRef(defn.ClassTagClass))
        formal.argTypes match {
          case arg :: Nil =>
            fullyDefinedType(arg, "ClassTag argument", pos) match {
              case defn.ArrayOf(elemTp) =>
                val etag = inferImplicitArg(defn.ClassTagType.appliedTo(elemTp), error, pos)
                if (etag.isEmpty) etag else etag.select(nme.wrap)
              case tp if hasStableErasure(tp) =>
                if (defn.isBottomClass(tp.typeSymbol))
                  error(where => i"attempt to take ClassTag of undetermined type for $where")
                ref(defn.ClassTagModule)
                  .select(nme.apply)
                  .appliedToType(tp)
                  .appliedTo(clsOf(erasure(tp)))
                  .withPos(pos)
              case tp =>
                EmptyTree
            }
          case _ =>
            EmptyTree
        }
      else EmptyTree
    }

    /** The context to be used when resolving a by-name implicit argument.
     *  This makes any implicit stored under `DelayedImplicit` visible and
     *  stores in turn the given `lazyImplicit` as new `DelayedImplicit`.
     */
    def lazyImplicitCtx(lazyImplicit: Symbol): Context = {
      val lctx = ctx.fresh
      for (delayedRef <- ctx.property(DelayedImplicit))
        lctx.setImplicits(new ContextualImplicits(delayedRef :: Nil, ctx.implicits)(ctx))
      lctx.setProperty(DelayedImplicit, lazyImplicit.termRef)
    }

    /** formalValue: The value type for which an implicit is searched
     *  lazyImplicit: An implicit symbol to install for nested by-name resolutions
     *  argCtx      : The context to be used for searching the implicit argument
     */
    val (formalValue, lazyImplicit, argCtx) = formal match {
      case ExprType(fv) =>
        val lazyImplicit = ctx.newLazyImplicit(fv)
        (fv, lazyImplicit, lazyImplicitCtx(lazyImplicit))
      case _ => (formal, NoSymbol, ctx)
    }

    inferImplicit(formalValue, EmptyTree, pos)(argCtx) match {
      case SearchSuccess(arg, _, _, _) =>
        def refersToLazyImplicit = arg.existsSubTree {
          case id: Ident => id.symbol == lazyImplicit
          case _ => false
        }
        if (lazyImplicit.exists && refersToLazyImplicit)
          Block(ValDef(lazyImplicit.asTerm, arg).withPos(pos) :: Nil, ref(lazyImplicit))
        else
          arg
      case ambi: AmbiguousImplicits =>
        error(where => s"ambiguous implicits: ${ambi.explanation} of $where")
        EmptyTree
      case failure: SearchFailure =>
        val arg = synthesizedClassTag(formalValue, pos)
        if (!arg.isEmpty) arg
        else {
          var msgFn = (where: String) =>
            em"no implicit argument of type $formal found for $where" + failure.postscript
          for {
            notFound <- formalValue.typeSymbol.getAnnotation(defn.ImplicitNotFoundAnnot)
            Trees.Literal(Constant(raw: String)) <- notFound.argument(0)
          } {
            msgFn = where =>
              err.implicitNotFoundString(
                raw,
                formalValue.typeSymbol.typeParams.map(_.name.unexpandedName.toString),
                formalValue.argInfos)
          }
          error(msgFn)
          EmptyTree
        }
    }
  }

  private def assumedCanEqual(ltp: Type, rtp: Type)(implicit ctx: Context) = {
    def eqNullable: Boolean = {
      val other =
        if (ltp.isRef(defn.NullClass)) rtp
        else if (rtp.isRef(defn.NullClass)) ltp
        else NoType

      (other ne NoType) && !other.derivesFrom(defn.AnyValClass)
    }

    val lift = new TypeMap {
      def apply(t: Type) = t match {
        case t: TypeRef =>
          t.info match {
            case TypeBounds(lo, hi) if lo ne hi => hi
            case _ => t
          }
        case _ =>
          if (variance > 0) mapOver(t) else t
      }
    }
    ltp.isError || rtp.isError || ltp <:< lift(rtp) || rtp <:< lift(ltp) || eqNullable
  }

  /** Check that equality tests between types `ltp` and `rtp` make sense */
  def checkCanEqual(ltp: Type, rtp: Type, pos: Position)(implicit ctx: Context): Unit =
    if (!ctx.isAfterTyper && !assumedCanEqual(ltp, rtp)) {
      val res = inferImplicitArg(
        defn.EqType.appliedTo(ltp, rtp), msgFun => ctx.error(msgFun(""), pos), pos)
      implicits.println(i"Eq witness found: $res: ${res.tpe}")
    }

  /** Find an implicit parameter or conversion.
   *  @param pt              The expected type of the parameter or conversion.
   *  @param argument        If an implicit conversion is searched, the argument to which
   *                         it should be applied, EmptyTree otherwise.
   *  @param pos             The position where errors should be reported.
   *  !!! todo: catch potential cycles
   */
  def inferImplicit(pt: Type, argument: Tree, pos: Position)(implicit ctx: Context): SearchResult = track("inferImplicit") {
    assert(!ctx.isAfterTyper,
      if (argument.isEmpty) i"missing implicit parameter of type $pt after typer"
      else i"type error: ${argument.tpe} does not conform to $pt${err.whyNoMatchStr(argument.tpe, pt)}")
    val prevConstr = ctx.typerState.constraint
    ctx.traceIndented(s"search implicit ${pt.show}, arg = ${argument.show}: ${argument.tpe.show}", implicits, show = true) {
      assert(!pt.isInstanceOf[ExprType])
      val isearch =
        if (ctx.settings.explaintypes.value) new ExplainedImplicitSearch(pt, argument, pos)
        else new ImplicitSearch(pt, argument, pos)
      val result = isearch.bestImplicit
      result match {
        case result: SearchSuccess =>
          result.tstate.commit()
          implicits.println(i"committing ${result.tstate.constraint} yielding ${ctx.typerState.constraint} ${ctx.typerState.hashesStr}")
          result
        case result: AmbiguousImplicits =>
          val deepPt = pt.deepenProto
          if (deepPt ne pt) inferImplicit(deepPt, argument, pos)
          else if (ctx.scala2Mode && !ctx.mode.is(Mode.OldOverloadingResolution)) {
            inferImplicit(pt, argument, pos)(ctx.addMode(Mode.OldOverloadingResolution)) match {
              case altResult: SearchSuccess =>
                ctx.migrationWarning(
                  s"According to new implicit resolution rules, this will be ambiguous:\n ${result.explanation}",
                  pos)
                altResult
              case _ =>
                result
            }
          }
          else result
        case _ =>
          assert(prevConstr eq ctx.typerState.constraint)
          result
      }
    }
  }

  /** An implicit search; parameters as in `inferImplicit` */
  class ImplicitSearch(protected val pt: Type, protected val argument: Tree, pos: Position)(implicit ctx: Context) {

    private def nestedContext = ctx.fresh.setMode(ctx.mode &~ Mode.ImplicitsEnabled)

    private def implicitProto(resultType: Type, f: Type => Type) =
      if (argument.isEmpty) f(resultType) else ViewProto(f(argument.tpe.widen), f(resultType))
        // Not clear whether we need to drop the `.widen` here. All tests pass with it in place, though.

    assert(argument.isEmpty || argument.tpe.isValueType || argument.tpe.isInstanceOf[ExprType],
        em"found: $argument: ${argument.tpe}, expected: $pt")

    /** The expected type for the searched implicit */
    lazy val fullProto = implicitProto(pt, identity)

    lazy val funProto = fullProto match {
      case proto: ViewProto =>
        FunProto(untpd.TypedSplice(dummyTreeOfType(proto.argType)) :: Nil, proto.resultType, self)
      case proto => proto
    }

    /** The expected type where parameters and uninstantiated typevars are replaced by wildcard types */
    val wildProto = implicitProto(pt, wildApprox(_, null, Set.empty))

    /** Search failures; overridden in ExplainedImplicitSearch */
    protected def nonMatchingImplicit(ref: TermRef, trail: List[MessageContainer]): SearchFailure = NoImplicitMatches
    protected def divergingImplicit(ref: TermRef): SearchFailure = NoImplicitMatches
    protected def shadowedImplicit(ref: TermRef, shadowing: Type): SearchFailure = NoImplicitMatches
    protected def failedSearch: SearchFailure = NoImplicitMatches

    /** Search a list of eligible implicit references */
    def searchImplicits(eligible: List[Candidate], contextual: Boolean): SearchResult = {
      val constr = ctx.typerState.constraint

      /** Try to typecheck an implicit reference */
      def typedImplicit(cand: Candidate)(implicit ctx: Context): SearchResult = track("typedImplicit") { ctx.traceIndented(i"typed implicit ${cand.ref}, pt = $pt, implicitsEnabled == ${ctx.mode is ImplicitsEnabled}", implicits, show = true) {
        assert(constr eq ctx.typerState.constraint)
        val ref = cand.ref
        var generated: Tree = tpd.ref(ref).withPos(pos)
        if (!argument.isEmpty)
          generated = typedUnadapted(
            untpd.Apply(untpd.TypedSplice(generated), untpd.TypedSplice(argument) :: Nil),
            pt)
        val generated1 = adapt(generated, pt)
        lazy val shadowing =
          typed(untpd.Ident(ref.name) withPos pos.toSynthetic, funProto)(
            nestedContext.addMode(Mode.ImplicitShadowing).setExploreTyperState)
        def refMatches(shadowing: Tree): Boolean =
          ref.symbol == closureBody(shadowing).symbol || {
            shadowing match {
              case Trees.Select(qual, nme.apply) => refMatches(qual)
              case _ => false
            }
          }
        // Does there exist an implicit value of type `Eq[tp, tp]`?
        def hasEq(tp: Type): Boolean =
          new ImplicitSearch(defn.EqType.appliedTo(tp, tp), EmptyTree, pos).bestImplicit match {
            case result: SearchSuccess => result.ref.symbol != defn.Predef_eqAny
            case result: AmbiguousImplicits => true
            case _ => false
          }

        def validEqAnyArgs(tp1: Type, tp2: Type) = {
          List(tp1, tp2).foreach(fullyDefinedType(_, "eqAny argument", pos))
          assumedCanEqual(tp1, tp2) || !hasEq(tp1) && !hasEq(tp2) ||
            { implicits.println(i"invalid eqAny[$tp1, $tp2]"); false }
        }
        if (ctx.reporter.hasErrors)
          nonMatchingImplicit(ref, ctx.reporter.removeBufferedMessages)
        else if (contextual && !ctx.mode.is(Mode.ImplicitShadowing) &&
                 !shadowing.tpe.isError && !refMatches(shadowing)) {
          implicits.println(i"SHADOWING $ref in ${ref.termSymbol.owner} is shadowed by $shadowing in ${shadowing.symbol.owner}")
          shadowedImplicit(ref, methPart(shadowing).tpe)
        }
        else generated1 match {
          case TypeApply(fn, targs @ (arg1 :: arg2 :: Nil))
          if fn.symbol == defn.Predef_eqAny && !validEqAnyArgs(arg1.tpe, arg2.tpe) =>
            nonMatchingImplicit(ref, Nil)
          case _ =>
            SearchSuccess(generated1, ref, cand.level, ctx.typerState)
        }
      }}

      /** Given a list of implicit references, produce a list of all implicit search successes,
       *  where the first is supposed to be the best one.
       *  @param pending   The list of implicit references that remain to be investigated
       *  @param acc       An accumulator of successful matches found so far.
       */
      def rankImplicits(pending: List[Candidate], acc: List[SearchSuccess]): List[SearchSuccess] = pending match {
        case cand :: pending1 =>
          val history = ctx.searchHistory nest wildProto
          val result =
            if (history eq ctx.searchHistory) divergingImplicit(cand.ref)
            else typedImplicit(cand)(nestedContext.setNewTyperState.setSearchHistory(history))
          result match {
            case fail: SearchFailure =>
              rankImplicits(pending1, acc)
            case best: SearchSuccess =>
              if (ctx.mode.is(Mode.ImplicitExploration)) best :: Nil
              else {
                val newPending = pending1.filter(cand1 =>
                  isAsGood(cand1.ref, best.ref, cand1.level, best.level)(nestedContext.setExploreTyperState))
                rankImplicits(newPending, best :: acc)
              }
          }
        case nil => acc
      }

      /** If the (result types of) the expected type, and both alternatives
       *  are all numeric value types, return the alternative which has
       *  the smaller numeric subtype as result type, if it exists.
       *  (This alternative is then discarded).
       */
      def numericValueTieBreak(alt1: SearchSuccess, alt2: SearchSuccess): SearchResult = {
        def isNumeric(tp: Type) = tp.typeSymbol.isNumericValueClass
        def isProperSubType(tp1: Type, tp2: Type) =
          tp1.isValueSubType(tp2) && !tp2.isValueSubType(tp1)
        val rpt = pt.resultType
        val rt1 = alt1.ref.widen.resultType
        val rt2 = alt2.ref.widen.resultType
        if (isNumeric(rpt) && isNumeric(rt1) && isNumeric(rt2))
          if (isProperSubType(rt1, rt2)) alt1
          else if (isProperSubType(rt2, rt1)) alt2
          else NoImplicitMatches
        else NoImplicitMatches
      }

      /** Convert a (possibly empty) list of search successes into a single search result */
      def condense(hits: List[SearchSuccess]): SearchResult = hits match {
        case best :: alts =>
          alts find (alt => isAsGood(alt.ref, best.ref, alt.level, best.level)(ctx.fresh.setExploreTyperState)) match {
            case Some(alt) =>
              typr.println(i"ambiguous implicits for $pt: ${best.ref} @ ${best.level}, ${alt.ref} @ ${alt.level}")
            /* !!! DEBUG
              println(i"ambiguous refs: ${hits map (_.ref) map (_.show) mkString ", "}")
              isAsGood(best.ref, alt.ref, explain = true)(ctx.fresh.withExploreTyperState)
            */
              numericValueTieBreak(best, alt) match {
                case eliminated: SearchSuccess => condense(hits.filter(_ ne eliminated))
                case _ => new AmbiguousImplicits(best.ref, alt.ref, pt, argument)
              }
            case None =>
              ctx.runInfo.useCount(best.ref) += 1
              best
          }
        case Nil =>
          failedSearch
      }

      def ranking(cand: Candidate) = -ctx.runInfo.useCount(cand.ref)

      /** Sort list of implicit references according to their popularity
       *  (# of times each was picked in current run).
       */
      def sort(eligible: List[Candidate]) = eligible match {
        case Nil => eligible
        case e1 :: Nil => eligible
        case e1 :: e2 :: Nil =>
          if (ranking(e2) < ranking(e1)) e2 :: e1 :: Nil
          else eligible
        case _ => eligible.sortBy(ranking)
      }

      condense(rankImplicits(sort(eligible), Nil))
    }

    /** Find a unique best implicit reference */
    def bestImplicit: SearchResult = {
      searchImplicits(ctx.implicits.eligible(wildProto), contextual = true) match {
        case result: SearchSuccess => result
        case result: AmbiguousImplicits => result
        case result: SearchFailure =>
          searchImplicits(implicitScope(wildProto).eligible, contextual = false)
      }
    }

    def implicitScope(tp: Type): OfTypeImplicits = ctx.runInfo.implicitScope(tp, ctx)
  }

  final class ExplainedImplicitSearch(pt: Type, argument: Tree, pos: Position)(implicit ctx: Context)
  extends ImplicitSearch(pt, argument, pos) {
    private var myFailures = new mutable.ListBuffer[ExplainedSearchFailure]
    private def record(fail: ExplainedSearchFailure) = {
      myFailures += fail
      fail
    }
    def failures = myFailures.toList
    override def nonMatchingImplicit(ref: TermRef, trail: List[MessageContainer]) =
      record(new NonMatchingImplicit(ref, pt, argument, trail))
    override def divergingImplicit(ref: TermRef) =
      record(new DivergingImplicit(ref, pt, argument))
    override def shadowedImplicit(ref: TermRef, shadowing: Type): SearchFailure =
      record(new ShadowedImplicit(ref, shadowing, pt, argument))
    override def failedSearch: SearchFailure = {
      //println(s"wildProto = $wildProto")
      //println(s"implicit scope = ${implicitScope(wildProto).companionRefs}")
      new FailedImplicit(failures, pt, argument)
    }
  }
}

/** Records the history of currently open implicit searches
 *  @param  searchDepth   The number of open searches.
 *  @param  seen          A map that records for each class symbol of a type
 *                        that's currently searched for the complexity of the
 *                        type that is searched for (wrt `typeSize`). The map
 *                        is populated only once `searchDepth` is greater than
 *                        the threshold given in the `XminImplicitSearchDepth` setting.
 */
class SearchHistory(val searchDepth: Int, val seen: Map[ClassSymbol, Int]) {

  /** The number of RefinementTypes in this type, after all aliases are expanded */
  private def typeSize(tp: Type)(implicit ctx: Context): Int = {
    val accu = new TypeAccumulator[Int] {
      def apply(n: Int, tp: Type): Int = tp match {
        case tp: RefinedType =>
          foldOver(n + 1, tp)
        case tp: TypeRef if tp.info.isAlias =>
          apply(n, tp.superType)
        case _ =>
          foldOver(n, tp)
      }
    }
    accu.apply(0, tp)
  }

  /** Check for possible divergence. If one is detected return the current search history
   *  (this will be used as a criterion to abandon the implicit search in rankImplicits).
   *  If no divergence is detected, produce a new search history nested in the current one
   *  which records that we are now also looking for type `proto`.
   *
   *  As long as `searchDepth` is lower than the `XminImplicitSearchDepth` value
   *  in settings, a new history is always produced, so the implicit search is always
   *  undertaken. If `searchDepth` matches or exceeds the `XminImplicitSearchDepth` value,
   *  we test that the new search is for a class that is either not yet in the set of
   *  `seen` classes, or the complexity of the type `proto` being searched for is strictly
   *  lower than the complexity of the type that was previously encountered and that had
   *  the same class symbol as `proto`. A possible divergence is detected if that test fails.
   */
  def nest(proto: Type)(implicit ctx: Context): SearchHistory = {
    if (searchDepth < ctx.settings.XminImplicitSearchDepth.value)
      new SearchHistory(searchDepth + 1, seen)
    else {
      val size = typeSize(proto)
      def updateMap(csyms: List[ClassSymbol], seen: Map[ClassSymbol, Int]): SearchHistory = csyms match {
        case csym :: csyms1 =>
          seen get csym match {
            // proto complexity is >= than the last time it was seen → diverge
            case Some(prevSize) if size >= prevSize => this
            case _ => updateMap(csyms1, seen.updated(csym, size))
          }
        case _ =>
          new SearchHistory(searchDepth + 1, seen)
      }
      if (proto.classSymbols.isEmpty) this
      else updateMap(proto.classSymbols, seen)
    }
  }
}

/** A set of term references where equality is =:= */
class TermRefSet(implicit ctx: Context) extends mutable.Traversable[TermRef] {
  import collection.JavaConverters._
  private val elems = (new java.util.LinkedHashMap[TermSymbol, List[Type]]).asScala

  def += (ref: TermRef): Unit = {
    val pre = ref.prefix
    val sym = ref.symbol.asTerm
    elems get sym match {
      case Some(prefixes) =>
        if (!(prefixes exists (_ =:= pre))) elems(sym) = pre :: prefixes
      case None =>
        elems(sym) = pre :: Nil
    }
  }

  def ++= (refs: TraversableOnce[TermRef]): Unit =
    refs foreach +=

  override def foreach[U](f: TermRef => U): Unit =
    for (sym <- elems.keysIterator)
      for (pre <- elems(sym))
        f(TermRef(pre, sym))
}

@sharable object EmptyTermRefSet extends TermRefSet()(NoContext)
