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
import NameKinds.LazyImplicitName
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
import reporting.diagnostic.{Message, MessageContainer}
import Inferencing.fullyDefinedType
import Trees._
import Hashable._
import util.Property
import config.Config
import config.Printers.{implicits, implicitsDetailed, typr}
import collection.mutable
import reporting.trace

/** Implicit resolution */
object Implicits {
  import tpd._

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

      def refMatches(ref: TermRef)(implicit ctx: Context) = /*trace(i"refMatches $ref $pt")*/ {

        def discardForView(tpw: Type, argType: Type): Boolean = tpw match {
          case mt: MethodType =>
            mt.isImplicitMethod ||
            mt.paramInfos.length != 1 ||
            !ctx.typerState.test(argType relaxed_<:< mt.paramInfos.head)
          case poly: PolyType =>
            // We do not need to call ProtoTypes#constrained on `poly` because
            // `refMatches` is always called with mode TypevarsMissContext enabled.
            poly.resultType match {
              case mt: MethodType =>
                mt.isImplicitMethod ||
                mt.paramInfos.length != 1 ||
                !ctx.typerState.test(argType relaxed_<:< wildApprox(mt.paramInfos.head, null, Set.empty))
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
            val isFunctionInS2 =
              ctx.scala2Mode && tpw.derivesFrom(defn.FunctionClass(1)) && ref.symbol != defn.Predef_conforms
            val isImplicitConverter = tpw.derivesFrom(defn.Predef_ImplicitConverter)
            val isConforms = // An implementation of <:< counts as a view, except that $conforms is always omitted
                tpw.derivesFrom(defn.Predef_Conforms) && ref.symbol != defn.Predef_conforms
            !(isFunctionInS2 || isImplicitConverter || isConforms)
        }

        def discardForValueType(tpw: Type): Boolean = tpw.stripPoly match {
          case tpw: MethodType => !tpw.isImplicitMethod
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
          else {
            val ptNorm = normalize(pt, pt) // `pt` could be implicit function types, check i2749
            NoViewsAllowed.isCompatible(normalize(ref, pt), ptNorm)
          }
        }
      }

      if (refs.isEmpty) Nil
      else {
        val nestedCtx = ctx.fresh.addMode(Mode.TypevarsMissContext)
        refs
          .filter(ref => nestedCtx.typerState.test(refMatches(ref)(nestedCtx)))
          .map(Candidate(_, level))
      }
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
        /*>|>*/ trace(i"eligible($tp), companions = ${companionRefs.toList}%, %", implicitsDetailed, show = true) /*<|<*/ {
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
               (ctx.scope eq outerImplicits.ctx.scope) &&
               !refs.head.name.is(LazyImplicitName)) outerImplicits.level
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

    private def computeEligible(tp: Type): List[Candidate] = /*>|>*/ trace(i"computeEligible $tp in $refs%, %", implicitsDetailed) /*<|<*/ {
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
    def tree: Tree
    def toText(printer: Printer): Text = printer.toText(this)
    def recoverWith(other: SearchFailure => SearchResult) = this match {
      case _: SearchSuccess => this
      case fail: SearchFailure => other(fail)
    }
    def isSuccess = isInstanceOf[SearchSuccess]
  }

  /** A successful search
   *  @param tree   The typed tree that needs to be inserted
   *  @param ref    The implicit reference that succeeded
   *  @param level  The level where the reference was found
   *  @param tstate The typer state to be committed if this alternative is chosen
   */
  case class SearchSuccess(tree: Tree, ref: TermRef, level: Int)(val tstate: TyperState) extends SearchResult with Showable

  /** A failed search */
  case class SearchFailure(tree: Tree) extends SearchResult {
    final def isAmbiguous = tree.tpe.isInstanceOf[AmbiguousImplicits]
    final def reason = tree.tpe.asInstanceOf[SearchFailureType]
  }

  object SearchFailure {
    def apply(tpe: SearchFailureType): SearchFailure = {
      val id =
        if (tpe.isInstanceOf[AmbiguousImplicits]) "/* ambiguous */"
        else "/* missing */"
      SearchFailure(untpd.SearchFailureIdent(id.toTermName).withTypeUnchecked(tpe))
    }
  }

  abstract class SearchFailureType extends ErrorType {
    def expectedType: Type
    protected def argument: Tree

    final protected def qualify(implicit ctx: Context) =
      if (expectedType.exists)
        if (argument.isEmpty) em"match type $expectedType"
        else em"convert from ${argument.tpe} to $expectedType"
      else
        if (argument.isEmpty) em"match expected type"
        else em"convert from ${argument.tpe} to expected type"

    /** An explanation of the cause of the failure as a string */
    def explanation(implicit ctx: Context): String

    def msg(implicit ctx: Context): Message = explanation

    /** If search was for an implicit conversion, a note describing the failure
     *  in more detail - this is either empty or starts with a '\n'
     */
    def whyNoConversion(implicit ctx: Context): String = ""
  }

  class NoMatchingImplicits(val expectedType: Type, val argument: Tree) extends SearchFailureType {
    def explanation(implicit ctx: Context): String =
      em"no implicit values were found that $qualify"
  }

  @sharable object NoMatchingImplicits extends NoMatchingImplicits(NoType, EmptyTree)

  @sharable val NoMatchingImplicitsFailure: SearchFailure =
    SearchFailure(NoMatchingImplicits)

  /** An ambiguous implicits failure */
  class AmbiguousImplicits(val alt1: SearchSuccess, val alt2: SearchSuccess, val expectedType: Type, val argument: Tree) extends SearchFailureType {
    def explanation(implicit ctx: Context): String =
      em"both ${err.refStr(alt1.ref)} and ${err.refStr(alt2.ref)} $qualify"
    override def whyNoConversion(implicit ctx: Context) =
      "\nNote that implicit conversions cannot be applied because they are ambiguous;" +
      "\n " + explanation
  }

  class MismatchedImplicit(ref: TermRef,
                           val expectedType: Type,
                           val argument: Tree) extends SearchFailureType {
    def explanation(implicit ctx: Context): String =
      em"${err.refStr(ref)} does not $qualify"
  }

  class ShadowedImplicit(ref: TermRef,
                         shadowing: Type,
                         val expectedType: Type,
                         val argument: Tree) extends SearchFailureType {
    def explanation(implicit ctx: Context): String =
      em"${err.refStr(ref)} does $qualify but is shadowed by ${err.refStr(shadowing)}"
  }

  class DivergingImplicit(ref: TermRef,
                          val expectedType: Type,
                          val argument: Tree) extends SearchFailureType {
    def explanation(implicit ctx: Context): String =
      em"${err.refStr(ref)} produces a diverging implicit search when trying to $qualify"
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
            AndType.make(tp, cls.typeRef.asSeenFrom(pre, cls.owner))
          val lead = if (tp.prefix eq NoPrefix) defn.AnyType else apply(tp.prefix)
          (lead /: tp.classSymbols)(joinClass)
        case tp: TypeVar =>
          apply(tp.underlying)
        case tp: AppliedType if !tp.tycon.typeSymbol.isClass =>
          def applyArg(arg: Type) = arg match {
            case TypeBounds(lo, hi) => AndType.make(lo, hi)
            case _: WildcardType => defn.AnyType
            case _ => arg
          }
          (apply(tp.tycon) /: tp.args)((tc, arg) => AndType.make(tc, applyArg(arg)))
        case tp: TypeLambda =>
          apply(tp.resType)
        case _ =>
          mapOver(tp)
      }
    }

    // todo: compute implicits directly, without going via companionRefs?
    def collectCompanions(tp: Type): TermRefSet = track("computeImplicitScope") {
      trace(i"collectCompanions($tp)", implicits) {

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
                  addRef(TermRef(companion, nme.PACKAGE))
                else if (compSym.exists)
                  comps += companion.asSeenFrom(pre, compSym.owner).asInstanceOf[TermRef]
              }
              def addParentScope(parent: Type): Unit =
                iscopeRefs(tp.baseType(parent.typeSymbol)) foreach addRef
              val companion = cls.companionModule
              if (companion.exists) addRef(companion.termRef)
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

  def clear() = {
    implicitScopeCache.clear()
  }
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
            (ctx.fresh.addMode(Mode.ImplicitExploration).setExploreTyperState()).isSuccess
          // TODO: investigate why we can't TyperState#test here
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
        || (from.tpe eq NoPrefix)) NoMatchingImplicitsFailure
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
  def inferImplicitArg(formal: Type, pos: Position)(implicit ctx: Context): Tree = {

    /** If `formal` is of the form ClassTag[T], where `T` is a class type,
     *  synthesize a class tag for `T`.
   	 */
    def synthesizedClassTag(formal: Type)(implicit ctx: Context): Tree =
      formal.argTypes match {
        case arg :: Nil =>
          fullyDefinedType(arg, "ClassTag argument", pos) match {
            case defn.ArrayOf(elemTp) =>
              val etag = inferImplicitArg(defn.ClassTagType.appliedTo(elemTp), pos)
              if (etag.tpe.isError) EmptyTree else etag.select(nme.wrap)
            case tp if hasStableErasure(tp) && !defn.isBottomClass(tp.typeSymbol) =>
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

    /** If `formal` is of the form Eq[T, U], where no `Eq` instance exists for
     *  either `T` or `U`, synthesize `Eq.eqAny[T, U]` as solution.
   	 */
    def synthesizedEq(formal: Type)(implicit ctx: Context): Tree = {
      //println(i"synth eq $formal / ${formal.argTypes}%, %")
      formal.argTypes match {
        case args @ (arg1 :: arg2 :: Nil)
        if !ctx.featureEnabled(defn.LanguageModuleClass, nme.strictEquality) &&
           ctx.typerState.test(validEqAnyArgs(arg1, arg2)) =>
          ref(defn.Eq_eqAny).appliedToTypes(args).withPos(pos)
        case _ =>
          EmptyTree
      }
    }

    def hasEq(tp: Type): Boolean =
      inferImplicit(defn.EqType.appliedTo(tp, tp), EmptyTree, pos).isSuccess

    def validEqAnyArgs(tp1: Type, tp2: Type)(implicit ctx: Context) = {
      List(tp1, tp2).foreach(fullyDefinedType(_, "eqAny argument", pos))
      assumedCanEqual(tp1, tp2) || !hasEq(tp1) && !hasEq(tp2)
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
      case SearchSuccess(arg, _, _) =>
        def refersToLazyImplicit = arg.existsSubTree {
          case id: Ident => id.symbol == lazyImplicit
          case _ => false
        }
        if (lazyImplicit.exists && refersToLazyImplicit)
          Block(ValDef(lazyImplicit.asTerm, arg).withPos(pos) :: Nil, ref(lazyImplicit))
        else
          arg
      case fail @ SearchFailure(tree) =>
        if (fail.isAmbiguous)
          tree
        else if (formalValue.isRef(defn.ClassTagClass))
          synthesizedClassTag(formalValue).orElse(tree)
        else if (formalValue.isRef(defn.EqClass))
          synthesizedEq(formalValue).orElse(tree)
        else
          tree
    }
  }

  /** Search an implicit argument and report error if not found */
  def implicitArgTree(formal: Type, pos: Position)(implicit ctx: Context): Tree = {
    val arg = inferImplicitArg(formal, pos)
    if (arg.tpe.isInstanceOf[SearchFailureType]) ctx.error(missingArgMsg(arg, formal, ""), pos)
    arg
  }

  def missingArgMsg(arg: Tree, pt: Type, where: String)(implicit ctx: Context): String = {
    def msg(shortForm: String)(headline: String = shortForm) = arg match {
      case arg: Trees.SearchFailureIdent[_] =>
        shortForm
      case _ =>
        i"""$headline.
           |I found:
           |
           |    ${arg.show.replace("\n", "\n    ")}
           |
           |But ${arg.tpe.asInstanceOf[SearchFailureType].explanation}."""
    }
    arg.tpe match {
      case ambi: AmbiguousImplicits =>
        msg(s"ambiguous implicit arguments: ${ambi.explanation} of $where")(
            s"ambiguous implicit arguments of type ${pt.show} found for $where")
      case _ =>
        val userDefined =
          for {
            notFound <- pt.typeSymbol.getAnnotation(defn.ImplicitNotFoundAnnot)
            Trees.Literal(Constant(raw: String)) <- notFound.argument(0)
          }
          yield {
            err.implicitNotFoundString(
              raw,
              pt.typeSymbol.typeParams.map(_.name.unexpandedName.toString),
              pt.argInfos)
          }
        msg(userDefined.getOrElse(em"no implicit argument of type $pt was found for $where"))()
    }
  }

  /** A string indicating the formal parameter corresponding to a  missing argument */
  def implicitParamString(paramName: TermName, methodStr: String, tree: Tree)(implicit ctx: Context): String =
    tree match {
      case Select(qual, nme.apply) if defn.isFunctionType(qual.tpe.widen) =>
        val qt = qual.tpe.widen
        val qt1 = qt.dealias
        def addendum = if (qt1 eq qt) "" else (i"\nwhich is an alias of: $qt1")
        em"parameter of ${qual.tpe.widen}$addendum"
      case _ =>
        em"parameter ${paramName} of $methodStr"
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
      def apply(t: Type): Type = t match {
        case t: TypeRef =>
          t.info match {
            case TypeBounds(lo, hi) if lo ne hi => apply(hi)
            case _ => t
          }
        case t: RefinedType =>
          apply(t.parent)
        case _ =>
          if (variance > 0) mapOver(t) else t
      }
    }
    ltp.isError || rtp.isError || ltp <:< lift(rtp) || rtp <:< lift(ltp) || eqNullable
  }

  /** Check that equality tests between types `ltp` and `rtp` make sense */
  def checkCanEqual(ltp: Type, rtp: Type, pos: Position)(implicit ctx: Context): Unit =
    if (!ctx.isAfterTyper && !assumedCanEqual(ltp, rtp)) {
      val res = implicitArgTree(defn.EqType.appliedTo(ltp, rtp), pos)
      implicits.println(i"Eq witness found for $ltp / $rtp: $res: ${res.tpe}")
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
    trace(s"search implicit ${pt.show}, arg = ${argument.show}: ${argument.tpe.show}", implicits, show = true) {
      assert(!pt.isInstanceOf[ExprType])
      val result = new ImplicitSearch(pt, argument, pos).bestImplicit(contextual = true)
      result match {
        case result: SearchSuccess =>
          result.tstate.commit()
          implicits.println(i"success: $result")
          implicits.println(i"committing ${result.tstate.constraint} yielding ${ctx.typerState.constraint} ${ctx.typerState.hashesStr}")
          result
        case result: SearchFailure if result.isAmbiguous =>
          val deepPt = pt.deepenProto
          if (deepPt ne pt) inferImplicit(deepPt, argument, pos)
          else if (ctx.scala2Mode && !ctx.mode.is(Mode.OldOverloadingResolution)) {
            inferImplicit(pt, argument, pos)(ctx.addMode(Mode.OldOverloadingResolution)) match {
              case altResult: SearchSuccess =>
                ctx.migrationWarning(
                  s"According to new implicit resolution rules, this will be ambiguous:\n ${result.reason.explanation}",
                  pos)
                altResult
              case _ =>
                result
            }
          }
          else result
        case _ =>
          result
      }
    }
  }

  /** An implicit search; parameters as in `inferImplicit` */
  class ImplicitSearch(protected val pt: Type, protected val argument: Tree, pos: Position)(implicit ctx: Context) {
    assert(argument.isEmpty || argument.tpe.isValueType || argument.tpe.isInstanceOf[ExprType],
        em"found: $argument: ${argument.tpe}, expected: $pt")

    private def nestedContext() = ctx.fresh.setMode(ctx.mode &~ Mode.ImplicitsEnabled)

    private def implicitProto(resultType: Type, f: Type => Type) =
      if (argument.isEmpty) f(resultType) else ViewProto(f(argument.tpe.widen), f(resultType))
        // Not clear whether we need to drop the `.widen` here. All tests pass with it in place, though.

    private def isCoherent = pt.isRef(defn.EqClass)

    private val cmpContext = nestedContext()
    private val cmpCandidates = (c1: Candidate, c2: Candidate) => compare(c1.ref, c2.ref, c1.level, c2.level)(cmpContext)

    /** The expected type for the searched implicit */
    lazy val fullProto = implicitProto(pt, identity)

    lazy val funProto = fullProto match {
      case proto: ViewProto =>
        FunProto(untpd.TypedSplice(dummyTreeOfType(proto.argType)) :: Nil, proto.resultType, self)
      case proto => proto
    }

    /** The expected type where parameters and uninstantiated typevars are replaced by wildcard types */
    val wildProto = implicitProto(pt, wildApprox(_, null, Set.empty))

    val isNot = wildProto.classSymbol == defn.NotClass

    /** Search a list of eligible implicit references */
    def searchImplicits(eligible: List[Candidate], contextual: Boolean): SearchResult = {
      val constr = ctx.typerState.constraint

      //println(i"search implicits $pt / ${eligible.map(_.ref)}")

      /** Try to typecheck an implicit reference */
      def typedImplicit(cand: Candidate)(implicit ctx: Context): SearchResult = track("typedImplicit") { trace(i"typed implicit ${cand.ref}, pt = $pt, implicitsEnabled == ${ctx.mode is ImplicitsEnabled}", implicits, show = true) {
        assert(constr eq ctx.typerState.constraint)
        val ref = cand.ref
        var generated: Tree = tpd.ref(ref).withPos(pos.startPos)
        if (!argument.isEmpty)
          generated = typedUnadapted(
            untpd.Apply(untpd.TypedSplice(generated), untpd.TypedSplice(argument) :: Nil),
            pt)
        val generated1 = adapt(generated, pt)
        lazy val shadowing =
          typed(untpd.Ident(ref.name) withPos pos.toSynthetic, funProto)(
            nestedContext().addMode(Mode.ImplicitShadowing).setExploreTyperState())
        def refSameAs(shadowing: Tree): Boolean =
          ref.symbol == closureBody(shadowing).symbol || {
            shadowing match {
              case Trees.Select(qual, nme.apply) => refSameAs(qual)
              case Trees.Apply(fn, _) => refSameAs(fn)
              case Trees.TypeApply(fn, _) => refSameAs(fn)
              case _ => false
            }
          }

        if (ctx.reporter.hasErrors) {
          ctx.reporter.removeBufferedMessages
          SearchFailure {
            generated1.tpe match {
              case _: SearchFailureType => generated1
              case _ => generated1.withType(new MismatchedImplicit(ref, pt, argument))
            }
          }
        }
        else if (contextual && !ctx.mode.is(Mode.ImplicitShadowing) &&
                 !shadowing.tpe.isError && !refSameAs(shadowing)) {
          implicits.println(i"SHADOWING $ref in ${ref.termSymbol.maybeOwner} is shadowed by $shadowing in ${shadowing.symbol.maybeOwner}")
          SearchFailure(generated1.withTypeUnchecked(
            new ShadowedImplicit(ref, methPart(shadowing).tpe, pt, argument)))
        }
        else
          SearchSuccess(generated1, ref, cand.level)(ctx.typerState)
      }}

      /** Try to type-check implicit reference, after checking that this is not
       *  a diverging search
       */
      def tryImplicit(cand: Candidate): SearchResult = {
        val history = ctx.searchHistory nest wildProto
        if (history eq ctx.searchHistory)
          SearchFailure(new DivergingImplicit(cand.ref, pt, argument))
        else
          typedImplicit(cand)(nestedContext().setNewTyperState().setSearchHistory(history))
      }

      /** Compare previous success with reference and level to determine which one would be chosen, if
       *  an implicit starting with the reference was found.
       */
      def compareCandidate(prev: SearchSuccess, ref: TermRef, level: Int): Int =
        if (prev.ref eq ref) 0
        else ctx.typerState.test(compare(prev.ref, ref, prev.level, level)(nestedContext()))

      /* Seems we don't need this anymore.
      def numericValueTieBreak(alt1: SearchSuccess, alt2: SearchSuccess) = {
        def isNumeric(tp: Type) = tp.typeSymbol.isNumericValueClass
        def isProperSubType(tp1: Type, tp2: Type) =
          tp1.isValueSubType(tp2) && !tp2.isValueSubType(tp1)
          val rpt = pt.resultType
          val rt1 = alt1.ref.widen.resultType
          val rt2 = alt2.ref.widen.resultType
        if (isNumeric(rpt) && isNumeric(rt1) && isNumeric(rt2))
          if (isProperSubType(rt1, rt2)) alt2
          else if (isProperSubType(rt2, rt1)) alt1
          else NoMatchingImplicitsFailure
        else NoMatchingImplicitsFailure
      }
      */

      /** If `alt1` is also a search success, try to disambiguate as follows:
       *    - If alt2 is preferred over alt1, pick alt2, otherwise return an
       *      ambiguous implicits error.
       */
      def disambiguate(alt1: SearchResult, alt2: SearchSuccess) = alt1 match {
        case alt1: SearchSuccess =>
          val diff = compareCandidate(alt1, alt2.ref, alt2.level)
          assert(diff <= 0)   // diff > 0 candidates should already have been eliminated in `rank`
          if (diff < 0) alt2
          else
            // numericValueTypeBreak(alt1, alt2) recoverWith
            SearchFailure(new AmbiguousImplicits(alt1, alt2, pt, argument))
        case _: SearchFailure => alt2
      }

      /** Faced with an ambiguous implicits failure `fail`, try to find another
       *  alternative among `pending` that is strictly better than both ambiguous
       *  alternatives.  If that fails, return `fail`
       */
      def healAmbiguous(pending: List[Candidate], fail: SearchFailure) = {
        val ambi = fail.reason.asInstanceOf[AmbiguousImplicits]
        val newPending = pending.filter(cand =>
          compareCandidate(ambi.alt1, cand.ref, cand.level) < 0 &&
          compareCandidate(ambi.alt2, cand.ref, cand.level) < 0)
        rank(newPending, fail, Nil).recoverWith(_ => fail)
      }

      /** Try to find a best matching implicit term among all the candidates in `pending`.
       *  @param pending   The list of candidates that remain to be tested
       *  @param found     The result obtained from previously tried candidates
       *  @param rfailures A list of all failures from previously tried candidates in reverse order
       *
       *  The scheme is to try candidates one-by-one. If a trial is successful:
       *   - if the query term is a `Not[T]` treat it a failure,
       *   - otherwise, if a previous search was also successful, handle the ambiguity
       *     in `disambiguate`,
       *   - otherwise, continue the search with all candidates that are not strictly
       *     worse than the succesful candidate.
       *  If a trial failed:
       *    - if the query term is a `Not[T]` treat it as a success,
       *    - otherwise, if the failure is an ambiguity, try to heal it (see @healAmbiguous)
       *      and return an ambiguous error otherwise. However, under Scala2 mode this is
       *      treated as a simple failure, with a warning that semantics will change.
       *    - otherwise add the failure to `rfailures` and continue testing the other candidates.
       */
      def rank(pending: List[Candidate], found: SearchResult, rfailures: List[SearchFailure]): SearchResult =
        pending match  {
          case cand :: remaining =>
            negateIfNot(tryImplicit(cand)) match {
              case fail: SearchFailure =>
                if (fail.isAmbiguous)
                  if (ctx.scala2Mode) {
                    val result = rank(remaining, found, NoMatchingImplicitsFailure :: rfailures)
                    if (result.isSuccess)
                      warnAmbiguousNegation(fail.reason.asInstanceOf[AmbiguousImplicits])
                    result
                  }
                  else healAmbiguous(remaining, fail)
                else rank(remaining, found, fail :: rfailures)
              case best: SearchSuccess =>
                if (ctx.mode.is(Mode.ImplicitExploration) || isCoherent)
                  best
                else disambiguate(found, best) match {
                  case retained: SearchSuccess =>
                    val newPending =
                      if (retained eq found) remaining
                      else remaining.filter(cand =>
                        compareCandidate(retained, cand.ref, cand.level) <= 0)
                    rank(newPending, retained, rfailures)
                  case fail: SearchFailure =>
                    healAmbiguous(remaining, fail)
                }
            }
          case nil =>
            if (rfailures.isEmpty) found
            else found.recoverWith(_ => rfailures.reverse.maxBy(_.tree.treeSize))
        }

      def negateIfNot(result: SearchResult) =
        if (isNot)
          result match {
            case _: SearchFailure =>
              SearchSuccess(ref(defn.Not_value), defn.Not_value.termRef, 0)(
                ctx.typerState.fresh().setCommittable(true))
            case _: SearchSuccess =>
              NoMatchingImplicitsFailure
          }
      else result

      def warnAmbiguousNegation(ambi: AmbiguousImplicits) =
        ctx.migrationWarning(
          i"""Ambiguous implicits ${ambi.alt1.ref.symbol.showLocated} and ${ambi.alt2.ref.symbol.showLocated}
             |seem to be used to implement a local failure in order to negate an implicit search.
             |According to the new implicit resolution rules this is no longer possible;
             |the search will fail with a global ambiguity error instead.
             |
             |Consider using the scala.implicits.Not class to implement similar functionality.""", pos)

      /** A relation that imfluences the order in which implicits are tried.
       *  We prefer (in order of importance)
       *   1. more deeply nested definitions
       *   2. definitions in subclasses
       *   3. definitions with fewer implicit parameters
       *  The reason for (3) is that we want to fail fast if the search type
       *  is underconstrained. So we look for "small" goals first, because that
       *  will give an ambiguity quickly.
       */
      def prefer(cand1: Candidate, cand2: Candidate): Boolean = {
        val level1 = cand1.level
        val level2 = cand2.level
        if (level1 > level2) return true
        if (level1 < level2) return false
        val sym1 = cand1.ref.symbol
        val sym2 = cand2.ref.symbol
        val ownerScore = compareOwner(sym1.maybeOwner, sym2.maybeOwner)
        if (ownerScore > 0) return true
        if (ownerScore < 0) return false
        val arity1 = sym1.info.firstParamTypes.length
        val arity2 = sym2.info.firstParamTypes.length
        if (arity1 < arity2) return true
        if (arity1 > arity2) return false
        false
      }

      /** Sort list of implicit references according to `prefer`.
       *  This is just an optimization that aims at reducing the average
       *  number of candidates to be tested.
       */
      def sort(eligible: List[Candidate]) = eligible match {
        case Nil => eligible
        case e1 :: Nil => eligible
        case e1 :: e2 :: Nil =>
          if (prefer(e2, e1)) e2 :: e1 :: Nil
          else eligible
        case _ =>
          eligible.sortWith(prefer)
      }

      rank(sort(eligible), NoMatchingImplicitsFailure, Nil)
    } // end searchImplicits

    /** Find a unique best implicit reference */
    def bestImplicit(contextual: Boolean): SearchResult = {
      val eligible =
        if (contextual) ctx.implicits.eligible(wildProto)
        else implicitScope(wildProto).eligible
      searchImplicits(eligible, contextual).recoverWith {
        failure => failure.reason match {
          case _: AmbiguousImplicits => failure
          case _ => if (contextual) bestImplicit(contextual = false) else failure
        }
      }
    }

    def implicitScope(tp: Type): OfTypeImplicits = ctx.runInfo.implicitScope(tp, ctx)
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

  /** The number of applications and refinements in this type, after all aliases are expanded */
  private def typeSize(tp: Type)(implicit ctx: Context): Int = {
    val accu = new TypeAccumulator[Int] {
      def apply(n: Int, tp: Type): Int = tp match {
        case tp: AppliedType =>
          foldOver(n + 1, tp)
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

  override def toString = s"SearchHistory(depth = $searchDepth, seen = $seen)"
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
        f(TermRef.withSym(pre, sym))
}

@sharable object EmptyTermRefSet extends TermRefSet()(NoContext)
