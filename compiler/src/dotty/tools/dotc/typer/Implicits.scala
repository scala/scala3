package dotty.tools
package dotc
package typer

import core._
import ast.{Trees, TreeTypeMap, untpd, tpd, DesugarEnums}
import util.Spans._
import util.Stats.{track, record, monitored}
import printing.{Showable, Printer}
import printing.Texts._
import Contexts._
import Types._
import Flags._
import TypeErasure.{erasure, hasStableErasure}
import Mode.ImplicitsEnabled
import NameOps._
import NameKinds.LazyImplicitName
import Symbols._
import Denotations._
import Types._
import Decorators._
import Names._
import StdNames._
import Constants._
import ProtoTypes._
import ErrorReporting._
import reporting.diagnostic.Message
import Inferencing.fullyDefinedType
import Trees._
import transform.SymUtils._
import transform.TypeUtils._
import transform.SyntheticMembers._
import Hashable._
import util.{Property, SourceFile, NoSource}
import config.Config
import config.Printers.{implicits, implicitsDetailed}
import collection.mutable
import reporting.trace
import annotation.tailrec

import scala.annotation.internal.sharable
import scala.annotation.threadUnsafe

/** Implicit resolution */
object Implicits {
  import tpd._

  /** An implicit definition `implicitRef` that is visible under a different name, `alias`.
   *  Gets generated if an implicit ref is imported via a renaming import.
   */
  class RenamedImplicitRef(val underlyingRef: TermRef, val alias: TermName) extends ImplicitRef {
    def implicitName(implicit ctx: Context): TermName = alias
  }

  /** An eligible implicit candidate, consisting of an implicit reference and a nesting level */
  case class Candidate(implicitRef: ImplicitRef, kind: Candidate.Kind, level: Int) {
    def ref: TermRef = implicitRef.underlyingRef

    def isExtension = (kind & Candidate.Extension) != 0
    def isConversion = (kind & Candidate.Conversion) != 0
  }
  object Candidate {
    type Kind = Int
    final val None = 0
    final val Value = 1
    final val Conversion = 2
    final val Extension = 4
  }

  /** A common base class of contextual implicits and of-type implicits which
   *  represents a set of references to implicit definitions.
   */
  abstract class ImplicitRefs(initctx: Context) {
    implicit val ctx: Context =
      if (initctx == NoContext) initctx else initctx retractMode Mode.ImplicitsEnabled

    /** The nesting level of this context. Non-zero only in ContextialImplicits */
    def level: Int = 0

    /** The implicit references */
    def refs: List[ImplicitRef]

    private[this] var SingletonClass: ClassSymbol = null

    /** Widen type so that it is neither a singleton type nor a type that inherits from scala.Singleton. */
    private def widenSingleton(tp: Type)(implicit ctx: Context): Type = {
      if (SingletonClass == null) SingletonClass = defn.SingletonClass
      val wtp = tp.widenSingleton
      if (wtp.derivesFrom(SingletonClass)) defn.AnyType else wtp
    }

    /** Return those references in `refs` that are compatible with type `pt`. */
    protected def filterMatching(pt: Type)(implicit ctx: Context): List[Candidate] = track("filterMatching") {

      def candidateKind(ref: TermRef)(implicit ctx: Context): Candidate.Kind = /*trace(i"candidateKind $ref $pt")*/ {

        def viewCandidateKind(tpw: Type, argType: Type, resType: Type): Candidate.Kind = {

          def methodCandidateKind(mt: MethodType, approx: Boolean) =
            if (mt.isImplicitMethod)
              viewCandidateKind(normalize(mt, pt), argType, resType)
            else if (mt.paramInfos.lengthCompare(1) == 0 && {
                  var formal = widenSingleton(mt.paramInfos.head)
                  if (approx) formal = wildApprox(formal)
                  ctx.test(implicit ctx => argType relaxed_<:< formal.widenExpr)
                })
              Candidate.Conversion
            else
              Candidate.None

          tpw match {
            case mt: MethodType =>
              methodCandidateKind(mt, approx = false)
            case poly: PolyType =>
              // We do not need to call ProtoTypes#constrained on `poly` because
              // `candidateKind` is always called with mode TypevarsMissContext enabled.
              poly.resultType match {
                case mt: MethodType =>
                  methodCandidateKind(mt, approx = true)
                case rtp =>
                  viewCandidateKind(wildApprox(rtp), argType, resType)
              }
            case tpw: TermRef =>
              Candidate.Conversion | Candidate.Extension // can't discard overloaded refs
            case tpw =>
              // Only direct instances of Function1 and direct or indirect instances of <:< are eligible as views.
              // However, Predef.$conforms is not eligible, because it is a no-op.
              //
              // In principle, it would be cleanest if only implicit methods qualified
              // as implicit conversions. We could achieve that by having standard conversions like
              // this in Predef:
              //
              //    implicit def convertIfConforms[A, B](x: A)(implicit ev: A <:< B): B = ev(a)
              //    implicit def convertIfConverter[A, B](x: A)(implicit ev: Conversion[A, B]): B = ev(a)
              //
              // (Once `<:<` inherits from `Conversion` we only need the 2nd one.)
              // But clauses like this currently slow down implicit search a lot, because
              // they are eligible for all pairs of types, and therefore are tried too often.
              // We emulate instead these conversions directly in the search.
              // The reason for leaving out `Predef_conforms` is that we know it adds
              // nothing since it only relates subtype with supertype.
              //
              // We keep the old behavior under -language:Scala2.
              val isFunctionInS2 =
                ctx.scala2Mode && tpw.derivesFrom(defn.FunctionClass(1)) && ref.symbol != defn.Predef_conforms
              val isImplicitConversion = tpw.derivesFrom(defn.ConversionClass)
              val isConforms = // An implementation of <:< counts as a view, except that $conforms is always omitted
                  tpw.derivesFrom(defn.SubTypeClass) &&
                    (defn.isNewCollections || // In 2.13, the type of `$conforms` changed from `A <:< A` to `A => A`
                     ref.symbol != defn.Predef_conforms)
              val hasExtensions = resType match {
                case SelectionProto(name, _, _, _) =>
                  tpw.memberBasedOnFlags(name, required = ExtensionMethod).exists
                case _ => false
              }
              val conversionKind =
                if (isFunctionInS2 || isImplicitConversion || isConforms) Candidate.Conversion
                else Candidate.None
              val extensionKind =
                if (hasExtensions) Candidate.Extension
                else Candidate.None
              conversionKind | extensionKind
          }
        }

        def valueTypeCandidateKind(tpw: Type): Candidate.Kind = tpw.stripPoly match {
          case tpw: MethodType =>
            if (tpw.isImplicitMethod) Candidate.Value else Candidate.None
          case _ =>
            Candidate.Value
        }

        /** Widen singleton arguments of implicit conversions to their underlying type.
         *  This is necessary so that they can be found eligible for the argument type.
         *  Note that we always take the underlying type of a singleton type as the argument
         *  type, so that we get a reasonable implicit cache hit ratio.
         */
        def adjustSingletonArg(tp: Type): Type = tp.widenSingleton match {
          case tp: PolyType =>
            val res = adjustSingletonArg(tp.resType)
            if (res `eq` tp.resType) tp else tp.derivedLambdaType(resType = res)
          case tp: MethodType =>
            tp.derivedLambdaType(paramInfos = tp.paramInfos.mapConserve(widenSingleton))
          case _ => tp
        }

        var ckind =
          if (!ref.symbol.isAccessibleFrom(ref.prefix)) Candidate.None
          else pt match {
            case pt: ViewProto =>
              viewCandidateKind(ref.widen, pt.argType, pt.resType)
            case _: ValueTypeOrProto =>
              if (defn.isFunctionType(pt)) Candidate.Value
              else valueTypeCandidateKind(ref.widen)
            case _ =>
              Candidate.Value
          }

        if (ckind == Candidate.None)
          record("discarded eligible")
        else {
          val ptNorm = normalize(pt, pt) // `pt` could be implicit function types, check i2749
          val refAdjusted =
            if (pt.isInstanceOf[ViewProto]) adjustSingletonArg(ref)
            else ref
          val refNorm = normalize(refAdjusted, pt)
          if (!NoViewsAllowed.isCompatible(refNorm, ptNorm))
            ckind = Candidate.None
        }
        ckind
      }


      if (refs.isEmpty) Nil
      else {
        val nestedCtx = ctx.fresh.addMode(Mode.TypevarsMissContext)

        def matchingCandidate(ref: ImplicitRef): Option[Candidate] =
          nestedCtx.test(implicit ctx => candidateKind(ref.underlyingRef)) match {
            case Candidate.None => None
            case ckind => Some(new Candidate(ref, ckind, level))
          }

        refs.flatMap(matchingCandidate)
      }
    }
  }

  /** The implicit references coming from the implicit scope of a type.
   *  @param tp              the type determining the implicit scope
   *  @param companionRefs   the companion objects in the implicit scope.
   */
  class OfTypeImplicits(tp: Type, val companionRefs: TermRefSet)(initctx: Context) extends ImplicitRefs(initctx) {
    assert(initctx.typer != null)
    @threadUnsafe lazy val refs: List[ImplicitRef] = {
      val buf = new mutable.ListBuffer[TermRef]
      for (companion <- companionRefs) buf ++= companion.implicitMembers(ImplicitOrImpliedOrGiven)
      buf.toList
    }

    /** The candidates that are eligible for expected type `tp` */
    @threadUnsafe lazy val eligible: List[Candidate] =
      /*>|>*/ track("eligible in tpe") /*<|<*/ {
        /*>|>*/ trace(i"eligible($tp), companions = ${companionRefs.toList}%, %", implicitsDetailed, show = true) /*<|<*/ {
          if (refs.nonEmpty && monitored) record(s"check eligible refs in tpe", refs.length)
          filterMatching(tp)
        }
      }

    override def toString: String =
      i"OfTypeImplicits($tp), companions = ${companionRefs.toList}%, %; refs = $refs%, %."
  }

  /** The implicit references coming from the context.
   *  @param refs      the implicit references made visible by the current context.
   *                   Note: The name of the reference might be different from the name of its symbol.
   *                   In the case of a renaming import a => b, the name of the reference is the renamed
   *                   name, b, whereas the name of the symbol is the original name, a.
   *  @param outerCtx  the next outer context that makes visible further implicits
   */
  class ContextualImplicits(val refs: List[ImplicitRef], val outerImplicits: ContextualImplicits)(initctx: Context) extends ImplicitRefs(initctx) {
    private val eligibleCache = new java.util.IdentityHashMap[Type, List[Candidate]]

    /** The level increases if current context has a different owner or scope than
     *  the context of the next-outer ImplicitRefs. This is however disabled under
     *  Scala2 mode, since we do not want to change the implicit disambiguation then.
     */
    override val level: Int =
      if (outerImplicits == null) 1
      else if (ctx.scala2Mode ||
               (ctx.owner eq outerImplicits.ctx.owner) &&
               (ctx.scope eq outerImplicits.ctx.scope) &&
               !refs.head.implicitName.is(LazyImplicitName)) outerImplicits.level
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
      else {
        val eligibles = eligibleCache.get(tp)
        if (eligibles != null) {
          def elided(ci: ContextualImplicits): Int = {
            val n = ci.refs.length
            if (ci.isOuterMost) n
            else n + elided(ci.outerImplicits)
          }
          if (monitored) record(s"elided eligible refs", elided(this))
          eligibles
        }
        else if (ctx eq NoContext) Nil
        else {
          val result = computeEligible(tp)
          eligibleCache.put(tp, result)
          result
        }
      }
    }

    private def computeEligible(tp: Type): List[Candidate] = /*>|>*/ trace(i"computeEligible $tp in $refs%, %", implicitsDetailed) /*<|<*/ {
      if (monitored) record(s"check eligible refs in ctx", refs.length)
      val ownEligible = filterMatching(tp)
      if (isOuterMost) ownEligible
      else ownEligible ::: {
        val shadowed = ownEligible.map(_.ref.implicitName).toSet
        outerImplicits.eligible(tp).filterNot(cand => shadowed.contains(cand.ref.implicitName))
      }
    }

    override def toString: String = {
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
    def recoverWith(other: SearchFailure => SearchResult): SearchResult = this match {
      case _: SearchSuccess => this
      case fail: SearchFailure => other(fail)
    }
    def isSuccess: Boolean = isInstanceOf[SearchSuccess]
  }

  /** A successful search
   *  @param tree   The typed tree that needs to be inserted
   *  @param ref    The implicit reference that succeeded
   *  @param level  The level where the reference was found
   *  @param tstate The typer state to be committed if this alternative is chosen
   */
  case class SearchSuccess(tree: Tree, ref: TermRef, level: Int)(val tstate: TyperState, val gstate: GadtConstraint) extends SearchResult with Showable

  /** A failed search */
  case class SearchFailure(tree: Tree) extends SearchResult {
    final def isAmbiguous: Boolean = tree.tpe.isInstanceOf[AmbiguousImplicits]
    final def reason: SearchFailureType = tree.tpe.asInstanceOf[SearchFailureType]
  }

  object SearchFailure {
    def apply(tpe: SearchFailureType)(implicit src: SourceFile): SearchFailure = {
      val id =
        if (tpe.isInstanceOf[AmbiguousImplicits]) "/* ambiguous */"
        else "/* missing */"
      SearchFailure(untpd.SearchFailureIdent(id.toTermName).withTypeUnchecked(tpe))
    }
  }

  abstract class SearchFailureType extends ErrorType {
    def expectedType: Type
    def argument: Tree

    /** A "massaging" function for displayed types to give better info in error diagnostics */
    def clarify(tp: Type)(implicit ctx: Context): Type = tp

    final protected def qualify(implicit ctx: Context): String = expectedType match {
      case SelectionProto(name, mproto, _, _) if !argument.isEmpty =>
        em"provide an extension method `$name` on ${argument.tpe}"
      case NoType =>
        if (argument.isEmpty) em"match expected type"
        else em"convert from ${argument.tpe} to expected type"
      case _ =>
        if (argument.isEmpty) em"match type ${clarify(expectedType)}"
        else em"convert from ${argument.tpe} to ${clarify(expectedType)}"
    }

    /** An explanation of the cause of the failure as a string */
    def explanation(implicit ctx: Context): String

    def msg(implicit ctx: Context): Message = explanation

    /** If search was for an implicit conversion, a note describing the failure
     *  in more detail - this is either empty or starts with a '\n'
     */
    def whyNoConversion(implicit ctx: Context): String = ""
  }

  class NoMatchingImplicits(val expectedType: Type, val argument: Tree, constraint: Constraint = OrderingConstraint.empty) extends SearchFailureType {

    /** Replace all type parameters in constraint by their bounds, to make it clearer
     *  what was expected
     */
    override def clarify(tp: Type)(implicit ctx: Context): Type = {
      def replace(implicit ctx: Context): Type = {
        val map = new TypeMap {
          def apply(t: Type): Type = t match {
            case t: TypeParamRef =>
              constraint.entry(t) match {
                case NoType => t
                case bounds: TypeBounds => ctx.typeComparer.fullBounds(t)
                case t1 => t1
              }
            case t: TypeVar =>
              t.instanceOpt.orElse(apply(t.origin))
            case _ =>
              mapOver(t)
          }
        }
        map(tp)
      }

      val ctx1 = ctx.fresh.setExploreTyperState()
      ctx1.typerState.constraint = constraint
      replace(ctx1)
    }

    def explanation(implicit ctx: Context): String =
      em"no implicit values were found that $qualify"
    override def toString = s"NoMatchingImplicits($expectedType, $argument)"
  }

  @sharable object NoMatchingImplicits extends NoMatchingImplicits(NoType, EmptyTree, OrderingConstraint.empty)

  @sharable val NoMatchingImplicitsFailure: SearchFailure =
    SearchFailure(NoMatchingImplicits)(NoSource)

  /** An ambiguous implicits failure */
  class AmbiguousImplicits(val alt1: SearchSuccess, val alt2: SearchSuccess, val expectedType: Type, val argument: Tree) extends SearchFailureType {
    def explanation(implicit ctx: Context): String =
      em"both ${err.refStr(alt1.ref)} and ${err.refStr(alt2.ref)} $qualify"
    override def whyNoConversion(implicit ctx: Context): String = {
      val what = if (expectedType.isInstanceOf[SelectionProto]) "extension methods" else "conversions"
      i"""
         |Note that implicit $what cannot be applied because they are ambiguous;
         |$explanation"""
    }
  }

  class MismatchedImplicit(ref: TermRef,
                           val expectedType: Type,
                           val argument: Tree) extends SearchFailureType {
    def explanation(implicit ctx: Context): String =
      em"${err.refStr(ref)} does not $qualify"
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
trait ImplicitRunInfo { self: Run =>

  private val implicitScopeCache = mutable.AnyRefMap[Type, OfTypeImplicits]()

  private val EmptyTermRefSet = new TermRefSet()(NoContext)

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
        case tp: TypeRef if !tp.symbol.isClass =>
          val pre = tp.prefix
          def joinClass(tp: Type, cls: ClassSymbol) =
            AndType.make(tp, cls.typeRef.asSeenFrom(pre, cls.owner))
          val lead = if (pre eq NoPrefix) defn.AnyType else apply(pre)
          (lead /: tp.classSymbols)(joinClass)
        case tp: TypeVar =>
          apply(tp.underlying)
        case tp: AppliedType if !tp.tycon.typeSymbol.isClass =>
          def applyArg(arg: Type) = arg match {
            case TypeBounds(lo, hi) => AndType.make(lo, hi)
            case WildcardType(TypeBounds(lo, hi)) => AndType.make(lo, hi)
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
            if (!tp.symbol.is(Package) || ctx.scala2Mode) {
              // Don't consider implicits in package prefixes unless under -language:Scala2
              val pre = tp.prefix
              comps ++= iscopeRefs(pre)
              def addRef(companion: TermRef): Unit = {
                val compSym = companion.symbol
                if (compSym is Package) {
                  assert(ctx.scala2Mode)
                  addRef(companion.select(nme.PACKAGE))
                }
                else if (compSym.exists)
                  comps += companion.asSeenFrom(pre, compSym.owner).asInstanceOf[TermRef]
              }
              def addCompanionOf(sym: Symbol) = {
                val companion = sym.companionModule
                if (companion.exists) addRef(companion.termRef)
              }
              def addClassScope(cls: ClassSymbol): Unit = {
                addCompanionOf(cls)
                for (parent <- cls.classParents; ref <- iscopeRefs(tp.baseType(parent.classSymbol)))
                  addRef(ref)
              }
              tp.classSymbols(liftingCtx).foreach(addClassScope)
            }
          case _ =>
            for (part <- tp.namedPartsWith(_.isType)) comps ++= iscopeRefs(part)
        }
        comps
      }
    }

   /** The implicit scope of type `tp`
     *  @param isLifted    Type `tp` is the result of a `liftToClasses` application
     */
    def iscope(tp: Type, isLifted: Boolean = false): OfTypeImplicits = {
      val canCache = Config.cacheImplicitScopes && tp.hash != NotCached && !tp.isProvisional
      def computeIScope() = {
        val liftedTp = if (isLifted) tp else liftToClasses(tp)
        val refs =
          if (liftedTp ne tp)
            iscope(liftedTp, isLifted = true).companionRefs
          else
            collectCompanions(tp)
        val result = new OfTypeImplicits(tp, refs)(ctx)
        if (canCache &&
            ((tp eq rootTp) ||          // first type traversed is always cached
             !incomplete.contains(tp))) // other types are cached if they are not incomplete
          implicitScopeCache(tp) = result
        result
      }
      if (canCache) implicitScopeCache.getOrElse(tp, computeIScope())
      else computeIScope()
    }

    iscope(rootTp)
  }

  protected def reset(): Unit = {
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
        || from.isInstanceOf[Super]
        || (from.tpe eq NoPrefix)) NoMatchingImplicitsFailure
    else {
      def adjust(to: Type) = to.stripTypeVar.widenExpr match {
        case SelectionProto(name, memberProto, compat, true) =>
          SelectionProto(name, memberProto, compat, privateOK = false)
        case tp => tp
      }
      try inferImplicit(adjust(to), from, from.span)
      catch {
        case ex: AssertionError =>
          implicits.println(s"view $from ==> $to")
          implicits.println(ctx.typerState.constraint.show)
          implicits.println(TypeComparer.explained(implicit ctx => from.tpe <:< to))
          throw ex
      }
    }
  }

  /** Handlers to synthesize implicits for special types */
  type SpecialHandler = (Type, Span) => Context => Tree
  type SpecialHandlers = List[(ClassSymbol, SpecialHandler)]

  lazy val synthesizedClassTag: SpecialHandler =
    (formal, span) => implicit ctx => formal.argInfos match {
      case arg :: Nil =>
        fullyDefinedType(arg, "ClassTag argument", span) match {
          case defn.ArrayOf(elemTp) =>
            val etag = inferImplicitArg(defn.ClassTagType.appliedTo(elemTp), span)
            if (etag.tpe.isError) EmptyTree else etag.select(nme.wrap)
          case tp if hasStableErasure(tp) && !defn.isBottomClass(tp.typeSymbol) =>
            val sym = tp.typeSymbol
            val classTag = ref(defn.ClassTagModule)
            val tag =
              if (sym == defn.UnitClass || sym == defn.AnyClass || sym == defn.AnyValClass)
                classTag.select(sym.name.toTermName)
              else
                classTag.select(nme.apply).appliedToType(tp).appliedTo(clsOf(erasure(tp)))
              tag.withSpan(span)
          case tp =>
            EmptyTree
        }
      case _ =>
        EmptyTree
    }

  /** Synthesize the tree for `'[T]` for an implicit `scala.quoted.Type[T]`.
   *  `T` is deeply dealiassed to avoid references to local type aliases.
   */
  lazy val synthesizedTypeTag: SpecialHandler =
    (formal, span) => implicit ctx => {
      def quotedType(t: Type) = {
        if (StagingContext.level == 0)
          ctx.compilationUnit.needsStaging = true // We will need to run ReifyQuotes
        ref(defn.InternalQuoted_typeQuote).appliedToType(t)
      }
      formal.argInfos match {
        case arg :: Nil  =>
          val deepDealias = new TypeMap {
            def apply(tp: Type): Type = mapOver(tp.dealias)
          }
          quotedType(deepDealias(arg))
        case _ =>
          EmptyTree
      }
    }

  lazy val synthesizedTastyContext: SpecialHandler =
    (formal, span) => implicit ctx =>
      if (ctx.inInlineMethod || enclosingInlineds.nonEmpty) ref(defn.TastyReflection_macroContext)
      else EmptyTree

  lazy val synthesizedTupleFunction: SpecialHandler =
    (formal, span) => implicit ctx => formal match {
      case AppliedType(_, funArgs @ fun :: tupled :: Nil) =>
        def functionTypeEqual(baseFun: Type, actualArgs: List[Type], actualRet: Type, expected: Type) = {
          expected =:= defn.FunctionOf(actualArgs, actualRet, defn.isImplicitFunctionType(baseFun), defn.isErasedFunctionType(baseFun))
        }
        val arity: Int = {
          if (defn.isErasedFunctionType(fun) || defn.isErasedFunctionType(fun)) -1 // TODO support?
          else if (defn.isFunctionType(fun)) {
            // TupledFunction[(...) => R, ?]
            fun.dropDependentRefinement.dealias.argInfos match {
              case funArgs :+ funRet if functionTypeEqual(fun, defn.tupleType(funArgs) :: Nil, funRet, tupled) =>
                // TupledFunction[(...funArgs...) => funRet, ?]
                funArgs.size
              case _ => -1
            }
          } else if (defn.isFunctionType(tupled)) {
            // TupledFunction[?, (...) => R]
            tupled.dropDependentRefinement.dealias.argInfos match {
              case tupledArgs :: funRet :: Nil =>
                defn.tupleTypes(tupledArgs.dealias) match {
                  case Some(funArgs) if functionTypeEqual(tupled, funArgs, funRet, fun) =>
                    // TupledFunction[?, ((...funArgs...)) => funRet]
                    funArgs.size
                  case _ => -1
                }
              case _ => -1
            }
          }
          else {
            // TupledFunction[?, ?]
            -1
          }
        }
        if (arity == -1)
          EmptyTree
        else if (arity <= Definitions.MaxImplementedFunctionArity)
          ref(defn.InternalTupleFunctionModule).select(s"tupledFunction$arity".toTermName).appliedToTypes(funArgs)
        else
          ref(defn.InternalTupleFunctionModule).select("tupledFunctionXXL".toTermName).appliedToTypes(funArgs)
      case _ =>
        EmptyTree
    }

  /** If `formal` is of the form Eql[T, U], try to synthesize an
    *  `Eql.eqlAny[T, U]` as solution.
    */
  lazy val synthesizedEq: SpecialHandler =
    (formal, span) => implicit ctx => {

      /** Is there an `Eql[T, T]` instance, assuming -strictEquality? */
      def hasEq(tp: Type)(implicit ctx: Context): Boolean = {
        val inst = inferImplicitArg(defn.EqlType.appliedTo(tp, tp), span)
        !inst.isEmpty && !inst.tpe.isError
      }

      /** Can we assume the eqlAny instance for `tp1`, `tp2`?
       *  This is the case if assumedCanEqual(tp1, tp2), or
       *  one of `tp1`, `tp2` has a reflexive `Eql` instance.
       */
      def validEqAnyArgs(tp1: Type, tp2: Type)(implicit ctx: Context) =
        assumedCanEqual(tp1, tp2) || {
          val nestedCtx = ctx.fresh.addMode(Mode.StrictEquality)
          !hasEq(tp1)(nestedCtx) && !hasEq(tp2)(nestedCtx)
        }

      /** Is an `Eql[cls1, cls2]` instance assumed for predefined classes `cls1`, cls2`? */
      def canComparePredefinedClasses(cls1: ClassSymbol, cls2: ClassSymbol): Boolean = {
        def cmpWithBoxed(cls1: ClassSymbol, cls2: ClassSymbol) =
          cls2 == defn.boxedType(cls1.typeRef).symbol ||
          cls1.isNumericValueClass && cls2.derivesFrom(defn.BoxedNumberClass)

        if (cls1.isPrimitiveValueClass)
          if (cls2.isPrimitiveValueClass)
            cls1 == cls2 || cls1.isNumericValueClass && cls2.isNumericValueClass
          else
            cmpWithBoxed(cls1, cls2)
        else if (cls2.isPrimitiveValueClass)
          cmpWithBoxed(cls2, cls1)
        else if (cls1 == defn.NullClass)
          cls1 == cls2 || cls2.derivesFrom(defn.ObjectClass)
        else if (cls2 == defn.NullClass)
          cls1.derivesFrom(defn.ObjectClass)
        else
          false
      }

      /** Some simulated `Eql` instances for predefined types. It's more efficient
       *  to do this directly instead of setting up a lot of `Eql` instances to
       *  interpret.
       */
      def canComparePredefined(tp1: Type, tp2: Type) =
        tp1.classSymbols.exists(cls1 =>
          tp2.classSymbols.exists(cls2 => canComparePredefinedClasses(cls1, cls2)))

      formal.argTypes match {
        case args @ (arg1 :: arg2 :: Nil) =>
          List(arg1, arg2).foreach(fullyDefinedType(_, "eq argument", span))
          if (canComparePredefined(arg1, arg2)
              ||
              !strictEquality &&
              ctx.test(implicit ctx => validEqAnyArgs(arg1, arg2)))
            ref(defn.Eql_eqlAny).appliedToTypes(args).withSpan(span)
          else EmptyTree
        case _ =>
          EmptyTree
      }
    }

  /** Creates a tree that will produce a ValueOf instance for the requested type.
   * An EmptyTree is returned if materialization fails.
   */
  lazy val synthesizedValueOf: SpecialHandler =
    (formal, span) => implicit ctx => {
      def success(t: Tree) = New(defn.ValueOfClass.typeRef.appliedTo(t.tpe), t :: Nil).withSpan(span)

      formal.argTypes match {
        case arg :: Nil =>
          fullyDefinedType(arg.dealias, "ValueOf argument", span) match {
            case ConstantType(c: Constant) =>
              success(Literal(c))
            case TypeRef(_, sym) if sym == defn.UnitClass =>
              success(Literal(Constant(())))
            case n: TermRef =>
              success(ref(n))
            case tp =>
              EmptyTree
          }
        case _ =>
          EmptyTree
      }
    }

  /** Create an anonymous class `new Object { type MirroredMonoType = ... }`
   *  and mark it with given attachment so that it is made into a mirror at PostTyper.
   */
  private def anonymousMirror(monoType: Type, attachment: Property.StickyKey[Unit], span: Span)(implicit ctx: Context) = {
    val monoTypeDef = untpd.TypeDef(tpnme.MirroredMonoType, untpd.TypeTree(monoType))
    val newImpl = untpd.Template(
      constr = untpd.emptyConstructor,
      parents = untpd.TypeTree(defn.ObjectType) :: Nil,
      derived = Nil,
      self = EmptyValDef,
      body = monoTypeDef :: Nil
    ).withAttachment(attachment, ())
    typed(untpd.New(newImpl).withSpan(span))
  }

  /** The mirror type
   *
   *     <parent> {
   *       MirroredMonoType = <monoType>
   *       MirroredTypeConstrictor = <tycon>
   *       MirroredLabel = <label> }
   */
  private def mirrorCore(parent: Type, monoType: Type, mirroredType: Type, label: Name)(implicit ctx: Context) = {
    parent
      .refinedWith(tpnme.MirroredMonoType, TypeAlias(monoType))
      .refinedWith(tpnme.MirroredType, TypeAlias(mirroredType))
      .refinedWith(tpnme.MirroredLabel, TypeAlias(ConstantType(Constant(label.toString))))
  }

  /** A path referencing the companion of class type `clsType` */
  private def companionPath(clsType: Type, span: Span)(implicit ctx: Context) = {
    val ref = pathFor(clsType.companionRef)
    assert(ref.symbol.is(Module) && ref.symbol.companionClass == clsType.classSymbol)
    ref.withSpan(span)
  }

  /** An implied instance for a type of the form `Mirror.Product { type MirroredType = T }`
   *  where `T` is a generic product type or a case object or an enum case.
   */
  lazy val synthesizedProductMirror: SpecialHandler =
    (formal, span) => implicit ctx => {
      def mirrorFor(mirroredType0: Type): Tree = {
        val mirroredType = mirroredType0.stripTypeVar
        mirroredType match {
          case AndType(tp1, tp2) =>
            mirrorFor(tp1).orElse(mirrorFor(tp2))
          case _ =>
            if (mirroredType.termSymbol.is(CaseVal)) {
              val module = mirroredType.termSymbol
              val modulePath = pathFor(mirroredType).withSpan(span)
              if (module.info.classSymbol.is(Scala2x)) {
                val mirrorType = mirrorCore(defn.Mirror_SingletonProxyType, mirroredType, mirroredType, module.name)
                val mirrorRef = New(defn.Mirror_SingletonProxyType, modulePath :: Nil)
                mirrorRef.cast(mirrorType)
              }
              else {
                val mirrorType = mirrorCore(defn.Mirror_SingletonType, mirroredType, mirroredType, module.name)
                modulePath.cast(mirrorType)
              }
            }
            else if (mirroredType.classSymbol.isGenericProduct) {
              val cls = mirroredType.classSymbol
              val accessors = cls.caseAccessors.filterNot(_.is(PrivateLocal))
              val elemLabels = accessors.map(acc => ConstantType(Constant(acc.name.toString)))
              val (monoType, elemsType) = mirroredType match {
                case mirroredType: HKTypeLambda =>
                  val elems =
                    mirroredType.derivedLambdaType(
                      resType = TypeOps.nestedPairs(accessors.map(mirroredType.memberInfo(_).widenExpr))
                    )
                  val AppliedType(tycon, _) = mirroredType.resultType
                  val monoType = AppliedType(tycon, mirroredType.paramInfos)
                  (monoType, elems)
                case _ =>
                  val elems = TypeOps.nestedPairs(accessors.map(mirroredType.memberInfo(_).widenExpr))
                  (mirroredType, elems)
              }
              val mirrorType =
                mirrorCore(defn.Mirror_ProductType, monoType, mirroredType, cls.name)
                  .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
                  .refinedWith(tpnme.MirroredElemLabels, TypeAlias(TypeOps.nestedPairs(elemLabels)))
              val mirrorRef =
                if (cls.is(Scala2x)) anonymousMirror(monoType, ExtendsProductMirror, span)
                else companionPath(mirroredType, span)
              mirrorRef.cast(mirrorType)
            }
            else EmptyTree
        }
      }

      formal.member(tpnme.MirroredType).info match {
        case TypeBounds(mirroredType, _) => mirrorFor(mirroredType)
        case other => EmptyTree
      }
    }

  /** An implied instance for a type of the form `Mirror.Sum { type MirroredType = T }`
   *  where `T` is a generic sum type.
   */
  lazy val synthesizedSumMirror: SpecialHandler =
    (formal, span) => implicit ctx => {
      formal.member(tpnme.MirroredType).info match {
        case TypeBounds(mirroredType0, _) =>
          val mirroredType = mirroredType0.stripTypeVar
          if (mirroredType.classSymbol.isGenericSum) {
            val cls = mirroredType.classSymbol
            val elemLabels = cls.children.map(c => ConstantType(Constant(c.name.toString)))

            def solve(sym: Symbol): Type = sym match {
              case caseClass: ClassSymbol =>
                assert(caseClass.is(Case))
                if (caseClass.is(Module))
                  caseClass.sourceModule.termRef
                else {
                  caseClass.primaryConstructor.info match {
                    case info: PolyType =>
                      // Compute the the full child type by solving the subtype constraint
                      // `C[X1, ..., Xn] <: P`, where
                      //
                      //   - P is the current `mirroredType`
                      //   - C is the child class, with type parameters X1, ..., Xn
                      //
                      // Contravariant type parameters are minimized, all other type parameters are maximized.
                      def instantiate(implicit ctx: Context) = {
                        val poly = constrained(info, untpd.EmptyTree)._1
                        val resType = poly.finalResultType
                        val target = mirroredType match {
                          case tp: HKTypeLambda => tp.resultType
                          case tp => tp
                        }
                        resType <:< target
                        val tparams = poly.paramRefs
                        val variances = caseClass.typeParams.map(_.paramVariance)
                        val instanceTypes = (tparams, variances).zipped.map((tparam, variance) =>
                          ctx.typeComparer.instanceType(tparam, fromBelow = variance < 0))
                        resType.substParams(poly, instanceTypes)
                      }
                      instantiate(ctx.fresh.setExploreTyperState().setOwner(caseClass))
                    case _ =>
                      caseClass.typeRef
                  }
                }
              case child => child.termRef
            }

            val (monoType, elemsType) = mirroredType match {
              case mirroredType: HKTypeLambda =>
                val elems = mirroredType.derivedLambdaType(
                  resType = TypeOps.nestedPairs(cls.children.map(solve))
                )
                val AppliedType(tycon, _) = mirroredType.resultType
                val monoType = AppliedType(tycon, mirroredType.paramInfos)
                (monoType, elems)
              case _ =>
                val elems = TypeOps.nestedPairs(cls.children.map(solve))
                (mirroredType, elems)
            }

            val mirrorType =
               mirrorCore(defn.Mirror_SumType, monoType, mirroredType, cls.name)
                .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
                .refinedWith(tpnme.MirroredElemLabels, TypeAlias(TypeOps.nestedPairs(elemLabels)))
            val mirrorRef =
              if (cls.linkedClass.exists && !cls.is(Scala2x)) companionPath(mirroredType, span)
              else anonymousMirror(monoType, ExtendsSumMirror, span)
            mirrorRef.cast(mirrorType)
          } else EmptyTree
        case _ => EmptyTree
      }
    }

  /** An implied instance for a type of the form `Mirror { type MirroredType = T }`
   *  where `T` is a generic sum or product or singleton type.
   */
  lazy val synthesizedMirror: SpecialHandler =
    (formal, span) => implicit ctx => {
      formal.member(tpnme.MirroredType).info match {
        case TypeBounds(mirroredType, _) =>
          if (mirroredType.termSymbol.is(CaseVal) || mirroredType.classSymbol.isGenericProduct)
            synthesizedProductMirror(formal, span)(ctx)
          else
            synthesizedSumMirror(formal, span)(ctx)
        case _ => EmptyTree
      }
    }

  private var mySpecialHandlers: SpecialHandlers = null

  private def specialHandlers(implicit ctx: Context) = {
    if (mySpecialHandlers == null)
      mySpecialHandlers = List(
        defn.ClassTagClass        -> synthesizedClassTag,
        defn.QuotedTypeClass      -> synthesizedTypeTag,
        defn.TastyReflectionClass -> synthesizedTastyContext,
        defn.EqlClass             -> synthesizedEq,
        defn.TupledFunctionClass -> synthesizedTupleFunction,
        defn.ValueOfClass         -> synthesizedValueOf,
        defn.Mirror_ProductClass  -> synthesizedProductMirror,
        defn.Mirror_SumClass      -> synthesizedSumMirror,
        defn.MirrorClass          -> synthesizedMirror
      )
    mySpecialHandlers
  }

  /** Find an implicit argument for parameter `formal`.
   *  Return a failure as a SearchFailureType in the type of the returned tree.
   */
  def inferImplicitArg(formal: Type, span: Span)(implicit ctx: Context): Tree = {
    inferImplicit(formal, EmptyTree, span)(ctx) match {
      case SearchSuccess(arg, _, _) => arg
      case fail @ SearchFailure(failed) =>
        def trySpecialCases(handlers: SpecialHandlers): Tree = handlers match {
          case (cls, handler) :: rest =>
            def baseWithRefinements(tp: Type): Type = tp.dealias match {
              case tp @ RefinedType(parent, rname, rinfo) =>
                tp.derivedRefinedType(baseWithRefinements(parent), rname, rinfo)
              case _ =>
                tp.baseType(cls)
            }
            val base = baseWithRefinements(formal)
            val result =
              if (base <:< formal) {
                // With the subtype test we enforce that the searched type `formal` is of the right form
                handler(base, span)(ctx)
              }
              else EmptyTree
            result.orElse(trySpecialCases(rest))
          case Nil =>
            failed
        }
      if (fail.isAmbiguous) failed
      else trySpecialCases(specialHandlers)
    }
  }

  /** Search an implicit argument and report error if not found */
  def implicitArgTree(formal: Type, span: Span)(implicit ctx: Context): Tree = {
    val arg = inferImplicitArg(formal, span)
    if (arg.tpe.isInstanceOf[SearchFailureType])
      ctx.error(missingArgMsg(arg, formal, ""), ctx.source.atSpan(span))
    arg
  }

  def missingArgMsg(arg: Tree, pt: Type, where: String)(implicit ctx: Context): String = {

    def msg(shortForm: String)(headline: String = shortForm) = arg match {
      case arg: Trees.SearchFailureIdent[_] =>
        shortForm
      case _ =>
        arg.tpe match {
          case tpe: SearchFailureType =>
            i"""$headline.
              |I found:
              |
              |    ${arg.show.replace("\n", "\n    ")}
              |
              |But ${tpe.explanation}."""
        }
    }

    def location(preposition: String) = if (where.isEmpty) "" else s" $preposition $where"

    /** Extract a user defined error message from a symbol `sym`
     *  with an annotation matching the given class symbol `cls`.
     */
    def userDefinedMsg(sym: Symbol, cls: Symbol) = for {
      ann <- sym.getAnnotation(cls)
      Trees.Literal(Constant(msg: String)) <- ann.argument(0)
    } yield msg


    arg.tpe match {
      case ambi: AmbiguousImplicits =>
        object AmbiguousImplicitMsg {
          def unapply(search: SearchSuccess): Option[String] =
            userDefinedMsg(search.ref.symbol, defn.ImplicitAmbiguousAnnot)
        }

        /** Construct a custom error message given an ambiguous implicit
         *  candidate `alt` and a user defined message `raw`.
         */
        def userDefinedAmbiguousImplicitMsg(alt: SearchSuccess, raw: String) = {
          val params = alt.ref.underlying match {
            case p: PolyType => p.paramNames.map(_.toString)
            case _           => Nil
          }
          def resolveTypes(targs: List[Tree])(implicit ctx: Context) =
            targs.map(a => fullyDefinedType(a.tpe, "type argument", a.span))

          // We can extract type arguments from:
          //   - a function call:
          //     @implicitAmbiguous("msg A=${A}")
          //     implicit def f[A](): String = ...
          //     implicitly[String] // found: f[Any]()
          //
          //   - an eta-expanded function:
          //     @implicitAmbiguous("msg A=${A}")
          //     implicit def f[A](x: Int): String = ...
          //     implicitly[Int => String] // found: x => f[Any](x)

          val call = closureBody(alt.tree) // the tree itself if not a closure
          val (_, targs, _) = decomposeCall(call)
          val args = resolveTypes(targs)(ctx.fresh.setTyperState(alt.tstate))
          err.userDefinedErrorString(raw, params, args)
        }

        (ambi.alt1, ambi.alt2) match {
          case (alt @ AmbiguousImplicitMsg(msg), _) =>
            userDefinedAmbiguousImplicitMsg(alt, msg)
          case (_, alt @ AmbiguousImplicitMsg(msg)) =>
            userDefinedAmbiguousImplicitMsg(alt, msg)
          case _ =>
            msg(s"ambiguous implicit arguments: ${ambi.explanation}${location("of")}")(
                s"ambiguous implicit arguments of type ${pt.show} found${location("for")}")
        }

      case _ =>
        val userDefined = userDefinedMsg(pt.typeSymbol, defn.ImplicitNotFoundAnnot).map(raw =>
          err.userDefinedErrorString(
            raw,
            pt.typeSymbol.typeParams.map(_.name.unexpandedName.toString),
            pt.argInfos))

        def hiddenImplicitsAddendum: String = arg.tpe match {
          case fail: SearchFailureType =>

            def hiddenImplicitNote(s: SearchSuccess) =
              em"\n\nNote: delegate ${s.ref.symbol.showLocated} was not considered because it was not imported with `import delegate`."

            def FindHiddenImplicitsCtx(ctx: Context): Context =
              if (ctx == NoContext) ctx
              else ctx.freshOver(FindHiddenImplicitsCtx(ctx.outer)).addMode(Mode.FindHiddenImplicits)

            inferImplicit(fail.expectedType, fail.argument, arg.span)(
              FindHiddenImplicitsCtx(ctx)) match {
              case s: SearchSuccess => hiddenImplicitNote(s)
              case f: SearchFailure =>
                f.reason match {
                  case ambi: AmbiguousImplicits => hiddenImplicitNote(ambi.alt1)
                  case r => ""
                }
            }
        }
        msg(userDefined.getOrElse(
          em"no implicit argument of type $pt was found${location("for")}"))() ++
        hiddenImplicitsAddendum
    }
  }

  /** A string indicating the formal parameter corresponding to a  missing argument */
  def implicitParamString(paramName: TermName, methodStr: String, tree: Tree)(implicit ctx: Context): String =
    tree match {
      case Select(qual, nme.apply) if defn.isFunctionType(qual.tpe.widen) =>
        val qt = qual.tpe.widen
        val qt1 = qt.dealiasKeepAnnots
        def addendum = if (qt1 eq qt) "" else (i"\nwhich is an alias of: $qt1")
        em"parameter of ${qual.tpe.widen}$addendum"
      case _ =>
        em"parameter ${paramName} of $methodStr"
    }

  private def strictEquality(implicit ctx: Context): Boolean =
    ctx.mode.is(Mode.StrictEquality) ||
    ctx.featureEnabled(nme.strictEquality)

  /** An Eql[T, U] instance is assumed
   *   - if one of T, U is an error type, or
   *   - if one of T, U is a subtype of the lifted version of the other,
   *     unless strict equality is set.
   */
  private def assumedCanEqual(ltp: Type, rtp: Type)(implicit ctx: Context) = {
    // Map all non-opaque abstract types to their upper bound.
    // This is done to check whether such types might plausibly be comparable to each other.
    val lift = new TypeMap {
      def apply(t: Type): Type = t match {
        case t: TypeRef =>
          t.info match {
            case TypeBounds(lo, hi) if lo.ne(hi) && !t.symbol.is(Opaque) => apply(hi)
            case _ => t
          }
        case t: RefinedType =>
          apply(t.parent)
        case _ =>
          if (variance > 0) mapOver(t) else t
      }
    }

    ltp.isError ||
    rtp.isError ||
    !strictEquality && {
      ltp <:< lift(rtp) ||
      rtp <:< lift(ltp)
    }
  }

  /** Check that equality tests between types `ltp` and `rtp` make sense */
  def checkCanEqual(ltp: Type, rtp: Type, span: Span)(implicit ctx: Context): Unit =
    if (!ctx.isAfterTyper && !assumedCanEqual(ltp, rtp)) {
      val res = implicitArgTree(defn.EqlType.appliedTo(ltp, rtp), span)
      implicits.println(i"Eql witness found for $ltp / $rtp: $res: ${res.tpe}")
    }

  /** Find an implicit parameter or conversion.
   *  @param pt              The expected type of the parameter or conversion.
   *  @param argument        If an implicit conversion is searched, the argument to which
   *                         it should be applied, EmptyTree otherwise.
   *  @param span            The position where errors should be reported.
   */
  def inferImplicit(pt: Type, argument: Tree, span: Span)(implicit ctx: Context): SearchResult = track("inferImplicit") {
    assert(ctx.phase.allowsImplicitSearch,
      if (argument.isEmpty) i"missing implicit parameter of type $pt after typer"
      else i"type error: ${argument.tpe} does not conform to $pt${err.whyNoMatchStr(argument.tpe, pt)}")
    trace(s"search implicit ${pt.show}, arg = ${argument.show}: ${argument.tpe.show}", implicits, show = true) {
      val result0 =
        try {
          new ImplicitSearch(pt, argument, span).bestImplicit(contextual = true)
        } catch {
          case ce: CyclicReference =>
            ce.inImplicitSearch = true
            throw ce
        }

      val result =
        result0 match {
          case result: SearchSuccess =>
            result.tstate.commit()
            ctx.gadt.restore(result.gstate)
            implicits.println(i"success: $result")
            implicits.println(i"committing ${result.tstate.constraint} yielding ${ctx.typerState.constraint} in ${ctx.typerState}")
            result
          case result: SearchFailure if result.isAmbiguous =>
            val deepPt = pt.deepenProto
            if (deepPt ne pt) inferImplicit(deepPt, argument, span)
            else if (ctx.scala2Mode && !ctx.mode.is(Mode.OldOverloadingResolution)) {
              inferImplicit(pt, argument, span)(ctx.addMode(Mode.OldOverloadingResolution)) match {
                case altResult: SearchSuccess =>
                  ctx.migrationWarning(
                    s"According to new implicit resolution rules, this will be ambiguous:\n${result.reason.explanation}",
                    ctx.source.atSpan(span))
                  altResult
                case _ =>
                  result
              }
            }
            else result
          case NoMatchingImplicitsFailure =>
            SearchFailure(new NoMatchingImplicits(pt, argument, ctx.typerState.constraint))
          case _ =>
            result0
        }
      // If we are at the outermost implicit search then emit the implicit dictionary, if any.
      ctx.searchHistory.emitDictionary(span, result)
    }
  }

  /** An implicit search; parameters as in `inferImplicit` */
  class ImplicitSearch(protected val pt: Type, protected val argument: Tree, span: Span)(implicit ctx: Context) {
    assert(argument.isEmpty || argument.tpe.isValueType || argument.tpe.isInstanceOf[ExprType],
        em"found: $argument: ${argument.tpe}, expected: $pt")

    private def nestedContext() =
      ctx.fresh.setMode(ctx.mode &~ Mode.ImplicitsEnabled)

    private def implicitProto(resultType: Type, f: Type => Type) =
      if (argument.isEmpty) f(resultType) else ViewProto(f(argument.tpe.widen), f(resultType))
        // Not clear whether we need to drop the `.widen` here. All tests pass with it in place, though.

    private def isCoherent = pt.isRef(defn.EqlClass)

    /** The expected type for the searched implicit */
    @threadUnsafe lazy val fullProto: Type = implicitProto(pt, identity)

    /** The expected type where parameters and uninstantiated typevars are replaced by wildcard types */
    val wildProto: Type = implicitProto(pt, wildApprox(_))

    val isNot: Boolean = wildProto.classSymbol == defn.NotClass

      //println(i"search implicits $pt / ${eligible.map(_.ref)}")

    /** Try to typecheck an implicit reference */
    def typedImplicit(cand: Candidate, contextual: Boolean)(implicit ctx: Context): SearchResult = track("typedImplicit") { trace(i"typed implicit ${cand.ref}, pt = $pt, implicitsEnabled == ${ctx.mode is ImplicitsEnabled}", implicits, show = true) {
      val ref = cand.ref
      val generated: Tree = tpd.ref(ref).withSpan(span.startPos)
      val locked = ctx.typerState.ownedVars
      val adapted =
        if (argument.isEmpty)
          adapt(generated, pt.widenExpr, locked)
        else {
          val untpdGenerated = untpd.TypedSplice(generated)
          def tryConversion(implicit ctx: Context) =
            typed(
              untpd.Apply(untpdGenerated, untpd.TypedSplice(argument) :: Nil),
              pt, locked)
          if (cand.isExtension) {
            val SelectionProto(name: TermName, mbrType, _, _) = pt
            val result = extMethodApply(untpd.Select(untpdGenerated, name), argument, mbrType)
            if (!ctx.reporter.hasErrors && cand.isConversion) {
              val testCtx = ctx.fresh.setExploreTyperState()
              tryConversion(testCtx)
              if (testCtx.reporter.hasErrors)
                ctx.error(em"ambiguous implicit: $generated is eligible both as an implicit conversion and as an extension method container")
            }
            result
          }
          else tryConversion
        }
      if (ctx.reporter.hasErrors) {
        ctx.reporter.removeBufferedMessages
        SearchFailure {
          adapted.tpe match {
            case _: SearchFailureType => adapted
            case _ => adapted.withType(new MismatchedImplicit(ref, pt, argument))
          }
        }
      }
      else {
        val returned =
          if (cand.isExtension) Applications.ExtMethodApply(adapted)
          else adapted
        SearchSuccess(returned, ref, cand.level)(ctx.typerState, ctx.gadt)
      }
    }}

    /** Try to type-check implicit reference, after checking that this is not
      * a diverging search
      */
    def tryImplicit(cand: Candidate, contextual: Boolean): SearchResult = {
      if (ctx.searchHistory.checkDivergence(cand, pt))
        SearchFailure(new DivergingImplicit(cand.ref, pt.widenExpr, argument))
      else {
        val history = ctx.searchHistory.nest(cand, pt)
        val result =
          typedImplicit(cand, contextual)(nestedContext().setNewTyperState().setFreshGADTBounds.setSearchHistory(history))
        result match {
          case res: SearchSuccess =>
            ctx.searchHistory.defineBynameImplicit(pt.widenExpr, res)
          case _ =>
            result
        }
      }
    }

    /** Search a list of eligible implicit references */
    def searchImplicits(eligible: List[Candidate], contextual: Boolean): SearchResult = {

      /** Compare previous success with reference and level to determine which one would be chosen, if
       *  an implicit starting with the reference was found.
       */
      def compareCandidate(prev: SearchSuccess, ref: TermRef, level: Int): Int =
        if (prev.ref eq ref) 0
        else if (prev.level != level) prev.level - level
        else nestedContext().test(implicit ctx => compare(prev.ref, ref))

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
       *     worse than the successful candidate.
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
            negateIfNot(tryImplicit(cand, contextual)) match {
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
                ctx.typerState.fresh().setCommittable(true),
                ctx.gadt
              )
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
             |Consider using the scala.implicits.Not class to implement similar functionality.""",
             ctx.source.atSpan(span))

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
      // Before searching for contextual or implicit scope candidates we first check if
      // there is an under construction or already constructed term with which we can tie
      // the knot.
      //
      // Since any suitable term found is defined as part of this search it will always be
      // effectively in a more inner context than any other definition provided by
      // explicit definitions. Consequently these terms have the highest priority and no
      // other candidates need to be considered.
      ctx.searchHistory.recursiveRef(pt) match {
        case ref: TermRef =>
          SearchSuccess(tpd.ref(ref).withSpan(span.startPos), ref, 0)(ctx.typerState, ctx.gadt)
        case _ =>
          val eligible =
            if (contextual) ctx.implicits.eligible(wildProto)
            else implicitScope(wildProto).eligible
          searchImplicits(eligible, contextual) match {
            case result: SearchSuccess =>
              result
            case failure: SearchFailure =>
              failure.reason match {
                case _: AmbiguousImplicits => failure
                case reason =>
                  if (contextual)
                    bestImplicit(contextual = false).recoverWith {
                      failure2 => failure2.reason match {
                        case _: AmbiguousImplicits => failure2
                        case _ =>
                          reason match {
                            case (_: DivergingImplicit) => failure
                            case _ => List(failure, failure2).maxBy(_.tree.treeSize)
                          }
                      }
                    }
                  else failure
              }
          }
      }
    }

    def implicitScope(tp: Type): OfTypeImplicits = ctx.run.implicitScope(tp, ctx)

    /** All available implicits, without ranking */
    def allImplicits: Set[TermRef] = {
      val contextuals = ctx.implicits.eligible(wildProto).map(tryImplicit(_, contextual = true))
      val inscope = implicitScope(wildProto).eligible.map(tryImplicit(_, contextual = false))
      (contextuals.toSet ++ inscope).collect {
        case success: SearchSuccess => success.ref
      }
    }
  }
}

/**
 * Records the history of currently open implicit searches.
 *
 * A search history maintains a list of open implicit searches (`open`) a shortcut flag
 * indicating whether any of these are by name (`byname`) and a reference to the root
 * search history (`root`) which in turn maintains a possibly empty dictionary of
 * recursive implicit terms constructed during this search.
 *
 * A search history provides operations to create a nested search history, check for
 * divergence, enter by name references and definitions in the implicit dictionary, lookup
 * recursive references and emit a complete implicit dictionary when the outermost search
 * is complete.
 */
abstract class SearchHistory { outer =>
  val root: SearchRoot
  val open: List[(Candidate, Type)]
  /** Does this search history contain any by name implicit arguments. */
  val byname: Boolean

  /**
   * Create the state for a nested implicit search.
   * @param cand The candidate implicit to be explored.
   * @param pt   The target type for the above candidate.
   * @result     The nested history.
   */
  def nest(cand: Candidate, pt: Type)(implicit ctx: Context): SearchHistory = {
    new SearchHistory {
      val root = outer.root
      val open = (cand, pt) :: outer.open
      val byname = outer.byname || isByname(pt)
    }
  }

  def isByname(tp: Type): Boolean = tp.isInstanceOf[ExprType]

  /**
   * Check if the supplied candidate implicit and target type indicate a diverging
   * implicit search.
   *
   * @param cand The candidate implicit to be explored.
   * @param pt   The target type for the above candidate.
   * @result     True if this candidate/pt are divergent, false otherwise.
   */
  def checkDivergence(cand: Candidate, pt: Type)(implicit ctx: Context): Boolean = {
    // For full details of the algorithm see the SIP:
    //   https://docs.scala-lang.org/sips/byname-implicits.html

    val widePt = pt.widenExpr
    lazy val ptCoveringSet = widePt.coveringSet
    lazy val ptSize = widePt.typeSize
    lazy val wildPt = wildApprox(widePt)

    // Unless we are able to tie a recursive knot, we report divergence if there is an
    // open implicit using the same candidate implicit definition which has a type which
    // is larger (see `typeSize`) and is constructed using the same set of types and type
    // constructors (see `coveringSet`).
    //
    // We are able to tie a recursive knot if there is compatible term already under
    // construction which is separated from this context by at least one by name argument
    // as we ascend the chain of open implicits to the outermost search context.

    @tailrec
    def loop(ois: List[(Candidate, Type)], belowByname: Boolean): Boolean =
      ois match {
        case Nil => false
        case (hd@(cand1, tp)) :: tl =>
          if (cand1.ref == cand.ref) {
            val wideTp = tp.widenExpr
            lazy val wildTp = wildApprox(wideTp)
            lazy val tpSize = wideTp.typeSize
            if (belowByname && (wildTp <:< wildPt)) false
            else if (tpSize > ptSize || wideTp.coveringSet != ptCoveringSet) loop(tl, isByname(tp) || belowByname)
            else tpSize < ptSize || wildTp =:= wildPt || loop(tl, isByname(tp) || belowByname)
          }
          else loop(tl, isByname(tp) || belowByname)
      }

    loop(open, isByname(pt))
  }

  /**
   * Return the reference, if any, to a term under construction or already constructed in
   * the current search history corresponding to the supplied target type.
   *
   * A term is eligible if its type is a subtype of the target type and either it has
   * already been constructed and is present in the current implicit dictionary, or it is
   * currently under construction and is separated from the current search context by at
   * least one by name argument position.
   *
   * Note that because any suitable term found is defined as part of this search it will
   * always be effectively in a more inner context than any other definition provided by
   * explicit definitions. Consequently these terms have the highest priority and no other
   * candidates need to be considered.
   *
   * @param pt  The target type being searched for.
   * @result    The corresponding dictionary reference if any, NoType otherwise.
   */
  def recursiveRef(pt: Type)(implicit ctx: Context): Type = {
    val widePt = pt.widenExpr

    refBynameImplicit(widePt).orElse {
      val bynamePt = isByname(pt)
      if (!byname && !bynamePt) NoType // No recursion unless at least one open implicit is by name ...
      else {
        // We are able to tie a recursive knot if there is compatible term already under
        // construction which is separated from this context by at least one by name
        // argument as we ascend the chain of open implicits to the outermost search
        // context.
        @tailrec
        def loop(ois: List[(Candidate, Type)], belowByname: Boolean): Type = {
          ois match {
            case (hd@(cand, tp)) :: tl if (belowByname || isByname(tp)) && tp.widenExpr <:< widePt => tp
            case (_, tp) :: tl => loop(tl, belowByname || isByname(tp))
            case _ => NoType
          }
        }

        loop(open, bynamePt) match {
          case NoType => NoType
          case tp => ctx.searchHistory.linkBynameImplicit(tp.widenExpr)
        }
      }
    }
  }

  // The following are delegated to the root of this search history.
  def linkBynameImplicit(tpe: Type)(implicit ctx: Context): TermRef = root.linkBynameImplicit(tpe)
  def refBynameImplicit(tpe: Type)(implicit ctx: Context): Type = root.refBynameImplicit(tpe)
  def defineBynameImplicit(tpe: Type, result: SearchSuccess)(implicit ctx: Context): SearchResult = root.defineBynameImplicit(tpe, result)

  // This is NOOP unless at the root of this search history.
  def emitDictionary(span: Span, result: SearchResult)(implicit ctx: Context): SearchResult = result

  override def toString: String = s"SearchHistory(open = $open, byname = $byname)"
}

/**
 * The the state corresponding to the outermost context of an implicit searcch.
 */
final class SearchRoot extends SearchHistory {
  val root = this
  val open = Nil
  val byname = false

  /** The dictionary of recursive implicit types and corresponding terms for this search. */
  var implicitDictionary0: mutable.Map[Type, (TermRef, tpd.Tree)] = null
  def implicitDictionary = {
    if (implicitDictionary0 == null)
      implicitDictionary0 = mutable.Map.empty[Type, (TermRef, tpd.Tree)]
    implicitDictionary0
  }

  /**
   * Link a reference to an under-construction implicit for the provided type to its
   * defining occurrence via the implicit dictionary, creating a dictionary entry for this
   * type if one does not yet exist.
   *
   * @param tpe  The type to link.
   * @result     The TermRef of the corresponding dictionary entry.
   */
  override def linkBynameImplicit(tpe: Type)(implicit ctx: Context): TermRef = {
    implicitDictionary.get(tpe) match {
      case Some((ref, _)) => ref
      case None =>
        val lazyImplicit = ctx.newLazyImplicit(tpe)
        val ref = lazyImplicit.termRef
        implicitDictionary.put(tpe, (ref, tpd.EmptyTree))
        ref
    }
  }

  /**
   * Look up an implicit dictionary entry by type.
   *
   * If present yield the TermRef corresponding to the eventual dictionary entry,
   * otherwise NoType.
   *
   * @param tpe The type to look up.
   * @result    The corresponding TermRef, or NoType if none.
   */
  override def refBynameImplicit(tpe: Type)(implicit ctx: Context): Type = {
    implicitDictionary.get(tpe).map(_._1).getOrElse(NoType)
  }

  /**
   * Define a pending dictionary entry if any.
   *
   * If the provided type corresponds to an under-construction by name implicit, then use
   * the tree contained in the provided SearchSuccess as its definition, returning an
   * updated result referring to dictionary entry. Otherwise return the SearchSuccess
   * unchanged.
   *
   * @param  tpe    The type for which the entry is to be defined
   * @param  result The SearchSuccess corresponding to tpe
   * @result        A SearchResult referring to the newly created dictionary entry if tpe
   *                is an under-construction by name implicit, the provided result otherwise.
   */
  override def defineBynameImplicit(tpe: Type, result: SearchSuccess)(implicit ctx: Context): SearchResult = {
    implicitDictionary.get(tpe) match {
      case Some((ref, _)) =>
        implicitDictionary.put(tpe, (ref, result.tree))
        SearchSuccess(tpd.ref(ref).withSpan(result.tree.span), result.ref, result.level)(result.tstate, result.gstate)
      case None => result
    }
  }

  /**
   * Emit the implicit dictionary at the completion of an implicit search.
   *
   * @param span   The position at which the search is elaborated.
   * @param result The result of the search prior to substitution of recursive references.
   * @result       The elaborated result, comprising the implicit dictionary and a result tree
   *               substituted with references into the dictionary.
   */
  override def emitDictionary(span: Span, result: SearchResult)(implicit ctx: Context): SearchResult = {
    if (implicitDictionary == null || implicitDictionary.isEmpty) result
    else {
      result match {
        case failure: SearchFailure => failure
        case success @ SearchSuccess(tree, _, _) =>
          import tpd._

          // We might have accumulated dictionary entries for by name implicit arguments
          // which are not in fact used recursively either directly in the outermost result
          // term, or indirectly via other dictionary entries. We prune these out, recursively
          // eliminating entries until all remaining entries are at least transtively referred
          // to in the outermost result term.
          @tailrec
          def prune(trees: List[Tree], pending: List[(TermRef, Tree)], acc: List[(TermRef, Tree)]): List[(TermRef, Tree)] = pending match {
            case Nil => acc
            case ps =>
              val (in, out) = ps.partition {
                case (vref, rhs) =>
                  trees.exists(_.existsSubTree {
                    case id: Ident => id.symbol == vref.symbol
                    case _ => false
                  })
              }
              if (in.isEmpty) acc
              else prune(in.map(_._2) ++ trees, out, in ++ acc)
          }

          val pruned = prune(List(tree), implicitDictionary.map(_._2).toList, Nil)
          implicitDictionary0 = null
          if (pruned.isEmpty) result
          else {
            // If there are any dictionary entries remaining after pruning, construct a dictionary
            // class of the form,
            //
            // class <dictionary> {
            //   val $_lazy_implicit_$0 = ...
            //   ...
            //   val $_lazy_implicit_$n = ...
            // }
            //
            // Where the RHSs of the $_lazy_implicit_$n are the terms used to populate the dictionary
            // via defineByNameImplicit.
            //
            // The returned search result is then of the form,
            //
            // {
            //   class <dictionary> { ... }
            //   val $_lazy_implicit_$nn = new <dictionary>
            //   result.tree // with dictionary references substituted in
            // }

            val parents = List(defn.ObjectType, defn.SerializableType)
            val classSym = ctx.newNormalizedClassSymbol(ctx.owner, LazyImplicitName.fresh().toTypeName, Synthetic | Final, parents, coord = span)
            val vsyms = pruned.map(_._1.symbol)
            val nsyms = vsyms.map(vsym => ctx.newSymbol(classSym, vsym.name, EmptyFlags, vsym.info, coord = span).entered)
            val vsymMap = (vsyms zip nsyms).toMap

            val rhss = pruned.map(_._2)
            // Substitute dictionary references into dictionary entry RHSs
            val rhsMap = new TreeTypeMap(treeMap = {
              case id: Ident if vsymMap.contains(id.symbol) =>
                tpd.ref(vsymMap(id.symbol))
              case tree => tree
            })
            val nrhss = rhss.map(rhsMap(_))

            val vdefs = (nsyms zip nrhss) map {
              case (nsym, nrhs) => ValDef(nsym.asTerm, nrhs.changeNonLocalOwners(nsym))
            }

            val constr = ctx.newConstructor(classSym, Synthetic, Nil, Nil).entered
            val classDef = ClassDef(classSym, DefDef(constr), vdefs)

            val valSym = ctx.newLazyImplicit(classSym.typeRef, span)
            val inst = ValDef(valSym, New(classSym.typeRef, Nil))

            // Substitute dictionary references into outermost result term.
            val resMap = new TreeTypeMap(treeMap = {
              case id: Ident if vsymMap.contains(id.symbol) =>
                Select(tpd.ref(valSym), id.name)
              case tree => tree
            })

            val res = resMap(tree)

            val blk = Inliner.reposition(Block(classDef :: inst :: Nil, res), span)

            success.copy(tree = blk)(success.tstate, success.gstate)
          }
      }
    }
  }
}

/** A set of term references where equality is =:= */
final class TermRefSet(implicit ctx: Context) {
  private[this] val elems = new java.util.LinkedHashMap[TermSymbol, List[Type]]

  def += (ref: TermRef): Unit = {
    val pre = ref.prefix
    val sym = ref.symbol.asTerm
    elems.get(sym) match {
      case null =>
        elems.put(sym, pre :: Nil)
      case prefixes =>
        if (!prefixes.exists(_ =:= pre))
          elems.put(sym, pre :: prefixes)
    }
  }

  def ++= (that: TermRefSet): Unit =
    that.foreach(+=)

  def foreach[U](f: TermRef => U): Unit =
    elems.forEach((sym: TermSymbol, prefixes: List[Type]) =>
      prefixes.foreach(pre => f(TermRef(pre, sym))))

  // used only for debugging
  def toList: List[TermRef] = {
    val buffer = new mutable.ListBuffer[TermRef]
    foreach(tr => buffer += tr)
    buffer.toList
  }

  override def toString = toList.toString
}
