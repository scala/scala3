package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, untpd, tpd, TreeInfo}
import util.Positions._
import util.Stats.{track, record, monitored}
import printing.Showable
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
import Applications._
import ProtoTypes._
import ErrorReporting._
import Inferencing.fullyDefinedType
import Trees._
import Hashable._
import config.Config
import config.Printers._
import collection.mutable

/** Implicit resolution */
object Implicits {

  /** A common base class of contextual implicits and of-type implicits which
   *  represents a set of implicit references.
   */
  abstract class ImplicitRefs(initctx: Context) {
    implicit val ctx: Context =
      if (initctx == NoContext) initctx else initctx retractMode Mode.ImplicitsEnabled

    /** The implicit references */
    def refs: List[TermRef]

    /** Return those references in `refs` that are compatible with type `pt`. */
    protected def filterMatching(pt: Type)(implicit ctx: Context): List[TermRef] = track("filterMatching") {

      def refMatches(ref: TermRef)(implicit ctx: Context) = /*ctx.traceIndented(i"refMatches $ref $pt")*/ {

        def discardForView(tpw: Type, argType: Type): Boolean = tpw match {
          case mt: MethodType =>
            mt.isImplicit ||
            mt.paramTypes.length != 1 ||
            !(argType relaxed_<:< mt.paramTypes.head)(ctx.fresh.setExploreTyperState)
          case poly: PolyType =>
            // We do not need to call ProtoTypes#constrained on `poly` because
            // `refMatches` is always called with mode TypevarsMissContext enabled.
            poly.resultType match {
              case mt: MethodType =>
                mt.isImplicit ||
                mt.paramTypes.length != 1 ||
                !(argType relaxed_<:< wildApprox(mt.paramTypes.head)(ctx.fresh.setExploreTyperState))
              case rtp =>
                discardForView(wildApprox(rtp), argType)
            }
          case tpw: TermRef =>
            false // can't discard overloaded refs
          case tpw =>
            //if (ctx.typer.isApplicable(tp, argType :: Nil, resultType))
            //  println(i"??? $tp is applicable to $this / typeSymbol = ${tpw.typeSymbol}")
            !tpw.derivesFrom(defn.FunctionClass(1)) ||
            ref.symbol == defn.Predef_conforms //
              // as an implicit conversion, Predef.$conforms is a no-op, so exclude it
        }

        def discardForValueType(tpw: Type): Boolean = tpw match {
          case mt: MethodType => !mt.isImplicit
          case mt: PolyType => discardForValueType(tpw.resultType)
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

      if (refs.isEmpty) refs
      else refs filter (refMatches(_)(ctx.fresh.addMode(Mode.TypevarsMissContext).setExploreTyperState)) // create a defensive copy of ctx to avoid constraint pollution
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

    /** The implicit references that are eligible for expected type `tp` */
    lazy val eligible: List[TermRef] =
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
    private val eligibleCache = new mutable.AnyRefMap[Type, List[TermRef]]

    /** The implicit references that are eligible for type `tp`. */
    def eligible(tp: Type): List[TermRef] = /*>|>*/ track(s"eligible in ctx") /*<|<*/ {
      if (tp.hash == NotCached) computeEligible(tp)
      else eligibleCache get tp match {
        case Some(eligibles) =>
          def elided(ci: ContextualImplicits): Int = {
            val n = ci.refs.length
            if (ci.outerImplicits == NoContext.implicits) n
            else n + elided(ci.outerImplicits)
          }
          if (monitored) record(s"elided eligible refs", elided(this))
          eligibles
        case None =>
          val savedEphemeral = ctx.typerState.ephemeral
          ctx.typerState.ephemeral = false
          try {
            val result = computeEligible(tp)
            if (ctx.typerState.ephemeral) record("ephemeral cache miss: eligible")
            else eligibleCache(tp) = result
            result
          }
          finally ctx.typerState.ephemeral |= savedEphemeral
      }
    }

    private def computeEligible(tp: Type): List[TermRef] = /*>|>*/ ctx.traceIndented(i"computeEligible $tp in $refs%, %", implicitsDetailed) /*<|<*/ {
      if (monitored) record(s"check eligible refs in ctx", refs.length)
      val ownEligible = filterMatching(tp)
      if (outerImplicits == NoContext.implicits) ownEligible
      else ownEligible ::: {
        val shadowed = (ownEligible map (_.name)).toSet
        outerImplicits.eligible(tp) filterNot (ref => shadowed contains ref.name)
      }
    }

    override def toString = {
      val own = s"(implicits: ${refs mkString ","})"
      if (outerImplicits == NoContext.implicits) own else own + "\n " + outerImplicits
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
  abstract class SearchResult

  /** A successful search
   *  @param ref   The implicit reference that succeeded
   *  @param tree  The typed tree that needs to be inserted
   *  @param ctx   The context after the implicit search
   */
  case class SearchSuccess(tree: tpd.Tree, ref: TermRef, tstate: TyperState) extends SearchResult {
    override def toString = s"SearchSuccess($tree, $ref)"
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

  /** A search failure that can show information about the cause */
  abstract class ExplainedSearchFailure extends SearchFailure {
    protected def pt: Type
    protected def argument: tpd.Tree
    protected def qualify(implicit ctx: Context) =
      if (argument.isEmpty) d"match type $pt"
      else d"convert from ${argument.tpe} to $pt"

    /** An explanation of the cause of the failure as a string */
    def explanation(implicit ctx: Context): String
  }

  /** An ambiguous implicits failure */
  class AmbiguousImplicits(alt1: TermRef, alt2: TermRef, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      d"both ${err.refStr(alt1)} and ${err.refStr(alt2)} $qualify"
    override def postscript(implicit ctx: Context) =
      "\nNote that implicit conversions cannot be applied because they are ambiguous;" +
      "\n " + explanation
  }

  class NonMatchingImplicit(ref: TermRef, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      d"${err.refStr(ref)} does not $qualify"
  }

  class ShadowedImplicit(ref: TermRef, shadowing: Type, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      d"${err.refStr(ref)} does $qualify but is shadowed by ${err.refStr(shadowing)}"
  }

  class DivergingImplicit(ref: TermRef, val pt: Type, val argument: tpd.Tree) extends ExplainedSearchFailure {
    def explanation(implicit ctx: Context): String =
      d"${err.refStr(ref)} produces a diverging implicit search when trying to $qualify"
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

  private val implicitScopeCache = mutable.AnyRefMap[Type, OfTypeImplicits]()

  /** The implicit scope of a type `tp`
   *  @param liftingCtx   A context to be used when computing the class symbols of
   *                      a type. Types may contain type variables with their instances
   *                      recorded in the current context. To find out the instance of
   *                      a type variable, we need the current context, the current
   *                      runinfo context does not do.
   */
  def implicitScope(tp: Type, liftingCtx: Context): OfTypeImplicits = {

    val seen: mutable.Set[Type] = mutable.Set()

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
        case tp: TypeRef if tp.symbol.isLambdaTrait =>
          defn.AnyType
        case tp: TypeRef if tp.symbol.isAbstractOrAliasType =>
          val pre = tp.prefix
          def joinClass(tp: Type, cls: ClassSymbol) =
            if (cls.isLambdaTrait) tp
            else AndType.make(tp, cls.typeRef.asSeenFrom(pre, cls.owner))
          val lead = if (tp.prefix eq NoPrefix) defn.AnyType else apply(tp.prefix)
          (lead /: tp.classSymbols)(joinClass)
        case tp: TypeVar =>
          apply(tp.underlying)
        case _ =>
          mapOver(tp)
      }
    }

    def iscopeRefs(tp: Type): TermRefSet =
      if (seen contains tp) EmptyTermRefSet
      else {
        seen += tp
        iscope(tp).companionRefs
      }

    // todo: compute implicits directly, without going via companionRefs?
    def collectCompanions(tp: Type): TermRefSet = track("computeImplicitScope") {
      ctx.traceIndented(i"collectCompanions($tp)", implicits) {
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
            for (part <- tp.namedPartsWith(_.isType))
              comps ++= iscopeRefs(part)
        }
        comps
      }
    }

   /** The implicit scope of type `tp`
     *  @param isLifted    Type `tp` is the result of a `liftToClasses` application
     */
    def iscope(tp: Type, isLifted: Boolean = false): OfTypeImplicits = {
      def computeIScope(cacheResult: Boolean) = {
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
          if (ctx.typerState.ephemeral) record("ephemeral cache miss: implicitScope")
          else if (cacheResult) implicitScopeCache(tp) = result
          result
        }
        finally ctx.typerState.ephemeral |= savedEphemeral
      }

      if (tp.hash == NotCached || !Config.cacheImplicitScopes)
        computeIScope(cacheResult = false)
      else implicitScopeCache get tp match {
        case Some(is) => is
        case None =>
          // Implicit scopes are tricky to cache because of loops. For example
          // in `tests/pos/implicit-scope-loop.scala`, the scope of B contains
          // the scope of A which contains the scope of B. We break the loop
          // by returning EmptyTermRefSet in `collectCompanions` for types
          // that we have already seen, but this means that we cannot cache
          // the computed scope of A, it is incomplete.
          // Keeping track of exactly where these loops happen would require a
          // lot of book-keeping, instead we choose to be conservative and only
          // cache scopes before any type has been seen. This is unfortunate
          // because loops are very common for types in scala.collection.
          computeIScope(cacheResult = seen.isEmpty)
      }
    }

    iscope(tp)
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
    && from.isInstanceOf[ValueType]
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
        || (from.tpe eq NoPrefix)) NoImplicitMatches
    else
      try inferImplicit(to.stripTypeVar.widenExpr, from, from.pos)
      catch {
        case ex: AssertionError =>
          implicits.println(s"view $from ==> $to")
          implicits.println(ctx.typerState.constraint.show)
          implicits.println(TypeComparer.explained(implicit ctx => from.tpe <:< to))
          throw ex
      }
  }

  /** Find an implicit argument for parameter `formal`.
   *  @param error  An error handler that gets an error message parameter
   *                which is itself parameterized by another string,
   *                indicating where the implicit parameter is needed
   */
  def inferImplicitArg(formal: Type, error: (String => String) => Unit, pos: Position)(implicit ctx: Context): Tree =
    inferImplicit(formal, EmptyTree, pos) match {
      case SearchSuccess(arg, _, _) =>
        arg
      case ambi: AmbiguousImplicits =>
        error(where => s"ambiguous implicits: ${ambi.explanation} of $where")
        EmptyTree
      case failure: SearchFailure =>
        val arg = synthesizedClassTag(formal, pos)
        if (!arg.isEmpty) arg
        else {
          var msgFn = (where: String) =>
            d"no implicit argument of type $formal found for $where" + failure.postscript
          for {
            notFound <- formal.typeSymbol.getAnnotation(defn.ImplicitNotFoundAnnot)
            Trees.Literal(Constant(raw: String)) <- notFound.argument(0)
          } {
            msgFn = where =>
              err.implicitNotFoundString(
                raw,
                formal.typeSymbol.typeParams.map(_.name.unexpandedName.toString),
                formal.argInfos)
          }
          error(msgFn)
          EmptyTree
        }
    }

  /** If `formal` is of the form ClassTag[T], where `T` is a class type,
   *  synthesize a class tag for `T`.
   */
  def synthesizedClassTag(formal: Type, pos: Position)(implicit ctx: Context): Tree = {
    if (formal.isRef(defn.ClassTagClass))
      formal.argTypes match {
        case arg :: Nil =>
          val tp = fullyDefinedType(arg, "ClassTag argument", pos)
          tp.underlyingClassRef(refinementOK = false) match {
            case tref: TypeRef =>
              return ref(defn.ClassTagModule)
                .select(nme.apply)
                .appliedToType(tp)
                .appliedTo(clsOf(tref))
                .withPos(pos)
            case _ =>
          }
        case _ =>
      }
    EmptyTree
  }

  private def assumedCanEqual(ltp: Type, rtp: Type)(implicit ctx: Context) = {
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
    ltp.isError || rtp.isError || ltp <:< lift(rtp) || rtp <:< lift(ltp)
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
        d"found: $argument: ${argument.tpe}, expected: $pt")

    /** The expected type for the searched implicit */
    lazy val fullProto = implicitProto(pt, identity)

    lazy val funProto = fullProto match {
      case proto: ViewProto =>
        FunProto(untpd.TypedSplice(dummyTreeOfType(proto.argType)) :: Nil, proto.resultType, self)
      case proto => proto
    }

    /** The expected type where parameters and uninstantiated typevars are replaced by wildcard types */
    val wildProto = implicitProto(pt, wildApprox(_))

    /** Search failures; overridden in ExplainedImplicitSearch */
    protected def nonMatchingImplicit(ref: TermRef): SearchFailure = NoImplicitMatches
    protected def divergingImplicit(ref: TermRef): SearchFailure = NoImplicitMatches
    protected def shadowedImplicit(ref: TermRef, shadowing: Type): SearchFailure = NoImplicitMatches
    protected def failedSearch: SearchFailure = NoImplicitMatches

    /** Search a list of eligible implicit references */
    def searchImplicits(eligible: List[TermRef], contextual: Boolean): SearchResult = {
      val constr = ctx.typerState.constraint

      /** Try to typecheck an implicit reference */
      def typedImplicit(ref: TermRef)(implicit ctx: Context): SearchResult = track("typedImplicit") { ctx.traceIndented(i"typed implicit $ref, pt = $pt, implicitsEnabled == ${ctx.mode is ImplicitsEnabled}", implicits, show = true) {
        assert(constr eq ctx.typerState.constraint)
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
          nonMatchingImplicit(ref)
        else if (contextual && !ctx.mode.is(Mode.ImplicitShadowing) &&
                 !shadowing.tpe.isError && !refMatches(shadowing)) {
          implicits.println(i"SHADOWING $ref in ${ref.termSymbol.owner} is shadowed by $shadowing in ${shadowing.symbol.owner}")
          shadowedImplicit(ref, methPart(shadowing).tpe)
        }
        else generated1 match {
          case TypeApply(fn, targs @ (arg1 :: arg2 :: Nil))
          if fn.symbol == defn.Predef_eqAny && !validEqAnyArgs(arg1.tpe, arg2.tpe) =>
            nonMatchingImplicit(ref)
          case _ =>
            SearchSuccess(generated1, ref, ctx.typerState)
        }
      }}

      /** Given a list of implicit references, produce a list of all implicit search successes,
       *  where the first is supposed to be the best one.
       *  @param pending   The list of implicit references that remain to be investigated
       *  @param acc       An accumulator of successful matches found so far.
       */
      def rankImplicits(pending: List[TermRef], acc: List[SearchSuccess]): List[SearchSuccess] = pending match {
        case ref :: pending1 =>
          val history = ctx.searchHistory nest wildProto
          val result =
            if (history eq ctx.searchHistory) divergingImplicit(ref)
            else typedImplicit(ref)(nestedContext.setNewTyperState.setSearchHistory(history))
          result match {
            case fail: SearchFailure =>
              rankImplicits(pending1, acc)
            case best: SearchSuccess =>
              if (ctx.mode.is(Mode.ImplicitExploration)) best :: Nil
              else {
                val newPending = pending1 filter (isAsGood(_, best.ref)(nestedContext.setExploreTyperState))
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
          alts find (alt => isAsGood(alt.ref, best.ref)(ctx.fresh.setExploreTyperState)) match {
            case Some(alt) =>
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
    override def nonMatchingImplicit(ref: TermRef) =
      record(new NonMatchingImplicit(ref, pt, argument))
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
          apply(n, tp.info.bounds.hi)
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
            case Some(prevSize) if size >= prevSize => this
            case _ => updateMap(csyms1, seen.updated(csym, size))
          }
        case nil =>
          if (csyms.isEmpty) this
          else new SearchHistory(searchDepth + 1, seen)
      }
      updateMap(proto.classSymbols, seen)
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
