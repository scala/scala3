package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import StdNames.nme
import ast.tpd.*
import Decorators.*
import typer.ErrorReporting.errorType
import Names.TermName
import NameKinds.ExistentialBinderName
import NameOps.isImpureFunction
import CaptureSet.IdempotentCaptRefMap
import reporting.Message
import util.{SimpleIdentitySet, EqHashMap}
import util.Spans.NoSpan
import annotation.internal.sharable

/** A module defining three kinds of root capabilities
 *   - `cap` of kind `Global`: This is the global root capability. Among others it is
 *     used in the types of formal parameters, in type bounds, and in self types.
 *     `cap` does not subsume other capabilities, except in arguments of
 *     `withCapAsRoot` calls.
 *   - Instances of Fresh(hidden), of kind Fresh. These do subsume other capabilties in scope.
 *     They track with hidden sets which other capabilities were subsumed.
 *     Hidden sets are inspected by separation checking.
 *   - Instances of Result(binder), of kind Result. These are existentials associated with
 *     the result types of dependent methods. They don't subsume other capabilties.
 *
 *  Representation:
 *
 *   - `cap` is just the TermRef `scala.caps.cap` defined in the `caps` module
 *   - `Fresh` and `Result` instances are annotated types of `scala.caps.cap`
 *     with a special `root.Annot` annotation. The symbol of the annotation is
 *     `annotation.internal.rootCapability`. The annotation carries a kind, which provides
 *     a hidden set for Fresh instances and a binder method type for Result instances.
 *
 * Setup:
 *
 *  In the setup phase, `cap` instances in the result of a dependent function type
 *  or method type such as `(x: T): C^{cap}` are converted to `Result(binder)` instances,
 *  where `binder` refers to the method type. Most other cap instances are mapped to
 *  Fresh instances instead. For example the `cap` in the result of `T => C^{cap}`
 *  is mapped to a Fresh instance.
 *
 *  If one needs to use a dependent function type yet one still want to map `cap` to
 *  a fresh instance instead an existential root, one can achieve that by the use
 *  of a type alias. For instance, the following type creates an existential for `^`:
 *
 *       (x: A) => (C^{x}, D^)
 *
 *  By contrast, this variant creates a fresh instance instead:
 *
 *       type F[X] = (x: A) => (C^{x}, X)
 *       F[D^]
 *
 *  The trick is that the argument D^ is mapped to D^{fresh} before the `F` alias
 *  is expanded.
 */
object root:

  enum Kind:
    case Result(binder: MethodType)
    case Fresh(hidden: CaptureSet.HiddenSet)
    case Global

    override def equals(other: Any): Boolean =
      (this eq other.asInstanceOf[AnyRef]) || this.match
      case Kind.Result(b1) => other match
        case Kind.Result(b2) => b1 eq b2
        case _ => false
      case Kind.Fresh(h1) => other match
        case Kind.Fresh(h2) => h1 eq h2
        case _ => false
      case Kind.Global => false
  end Kind

  @sharable private var rootId = 0

  /** The annotation of a root instance */
  case class Annot(kind: Kind) extends Annotation:

    /** id printed under -uniqid, for debugging */
    val id =
      rootId += 1
      rootId

    override def symbol(using Context) = defn.RootCapabilityAnnot
    override def tree(using Context) = New(symbol.typeRef, Nil)
    override def derivedAnnotation(tree: Tree)(using Context): Annotation = this

    private var myOriginalKind = kind
    def originalBinder: MethodType = myOriginalKind.asInstanceOf[Kind.Result].binder

    def derivedAnnotation(binder: MethodType)(using Context): Annotation = kind match
      case Kind.Result(b) if b ne binder =>
        val ann = Annot(Kind.Result(binder))
        ann.myOriginalKind = myOriginalKind
        ann
      case _ =>
        this

    override def hash: Int = kind.hashCode
    override def eql(that: Annotation) = that match
      case Annot(kind) => this.kind eq kind
      case _ => false

    /** Special treatment of `SubstBindingMaps` which can change the binder of a
     *  Result instances
     */
    override def mapWith(tm: TypeMap)(using Context) = kind match
      case Kind.Result(binder) => tm match
        case tm: Substituters.SubstBindingMap[MethodType] @unchecked if tm.from eq binder =>
          derivedAnnotation(tm.to)
        case _ => this
      case _ => this
  end Annot

  def cap(using Context): TermRef = defn.captureRoot.termRef

  /** The type of fresh references */
  type Fresh = AnnotatedType

  object Fresh:
    /** Constructor and extractor methods for "fresh" capabilities */
    private def make(owner: Symbol)(using Context): CaptureRef =
      if ccConfig.useSepChecks then
        val hiddenSet = CaptureSet.HiddenSet(owner)
        val res = AnnotatedType(cap, Annot(Kind.Fresh(hiddenSet)))
        hiddenSet.owningCap = res
        //assert(hiddenSet.id != 3)
        res
      else
        cap

    def withOwner(owner: Symbol)(using Context): CaptureRef = make(owner)
    def apply()(using Context): CaptureRef = make(NoSymbol)

    def unapply(tp: AnnotatedType): Option[CaptureSet.HiddenSet] = tp.annot match
      case Annot(Kind.Fresh(hidden)) => Some(hidden)
      case _ => None
  end Fresh

  /** The type of existentially bound references */
  type Result = AnnotatedType

  object Result:
    def apply(binder: MethodType)(using Context): Result =
      val hiddenSet = CaptureSet.HiddenSet(NoSymbol)
      val res = AnnotatedType(cap, Annot(Kind.Result(binder)))
      hiddenSet.owningCap = res
      res

    def unapply(tp: Result)(using Context): Option[MethodType] = tp.annot match
      case Annot(Kind.Result(binder)) => Some(binder)
      case _ => None
  end Result

  def unapply(root: CaptureRef)(using Context): Option[Kind] = root match
    case root @ AnnotatedType(_, ann: Annot) => Some(ann.kind)
    case _ if root.isCap => Some(Kind.Global)
    case _ => None

  /** Map each occurrence of cap to a different Sep.Cap instance */
  class CapToFresh(owner: Symbol)(using Context) extends BiTypeMap, FollowAliasesMap:
    thisMap =>

    override def apply(t: Type) =
      if variance <= 0 then t
      else t match
        case t: CaptureRef if t.isCap =>
          Fresh.withOwner(owner)
        case t @ CapturingType(_, _) =>
          mapOver(t)
        case t @ AnnotatedType(parent, ann) =>
          val parent1 = this(parent)
          if ann.symbol.isRetains && ann.tree.toCaptureSet.containsCap then
            this(CapturingType(parent1, ann.tree.toCaptureSet))
          else
            t.derivedAnnotatedType(parent1, ann)
        case _ =>
          mapFollowingAliases(t)

    override def toString = "CapToFresh"

    lazy val inverse: BiTypeMap & FollowAliasesMap = new BiTypeMap with FollowAliasesMap:
      def apply(t: Type): Type = t match
        case t @ Fresh(_) => cap
        case t @ CapturingType(_, refs) => mapOver(t)
        case _ => mapFollowingAliases(t)

      def inverse = thisMap
      override def toString = thisMap.toString + ".inverse"

  end CapToFresh

  /** Maps cap to fresh */
  def capToFresh(tp: Type, owner: Symbol = NoSymbol)(using Context): Type =
    if ccConfig.useSepChecks then CapToFresh(owner)(tp) else tp

  /** Maps fresh to cap */
  def freshToCap(tp: Type)(using Context): Type =
    if ccConfig.useSepChecks then CapToFresh(NoSymbol).inverse(tp) else tp

  /** Map top-level free existential variables one-to-one to Fresh instances */
  def resultToFresh(tp: Type)(using Context): Type =
    val subst = new IdempotentCaptRefMap:
      val seen = EqHashMap[Annotation, CaptureRef]()
      var localBinders: SimpleIdentitySet[MethodType] = SimpleIdentitySet.empty

      def apply(t: Type): Type = t match
        case t @ Result(binder) =>
          if localBinders.contains(binder) then t // keep bound references
          else seen.getOrElseUpdate(t.annot, Fresh()) // map free references to Fresh()
        case t: MethodType =>
          // skip parameters
          val saved = localBinders
          if t.marksExistentialScope then localBinders = localBinders + t
          try t.derivedLambdaType(resType = this(t.resType))
          finally localBinders = saved
        case t: PolyType =>
          // skip parameters
          t.derivedLambdaType(resType = this(t.resType))
        case _ =>
          mapOver(t)

    subst(tp)
  end resultToFresh

  /** Replace all occurrences of `cap` (or fresh) in parts of this type by an existentially bound
   *  variable bound by `mt`.
   *  Stop at function or method types since these have been mapped before.
   */
  def toResult(tp: Type, mt: MethodType, fail: Message => Unit)(using Context): Type =

    abstract class CapMap extends BiTypeMap:
      override def mapOver(t: Type): Type = t match
        case t @ FunctionOrMethod(args, res) if variance > 0 && !t.isAliasFun =>
          t // `t` should be mapped in this case by a different call to `mapCap`.
        case t: (LazyRef | TypeVar) =>
          mapConserveSuper(t)
        case _ =>
          super.mapOver(t)

    object toVar extends CapMap:
      private val seen = EqHashMap[CaptureRef, Result]()

      def apply(t: Type) = t match
        case t: CaptureRef if t.isCapOrFresh =>
          if variance > 0 then
            seen.getOrElseUpdate(t, Result(mt))
          else
            if variance == 0 then
              fail(em"""$tp captures the root capability `cap` in invariant position.
                       |This capability cannot be converted to an existential in the result type of a function.""")
            // we accept variance < 0, and leave the cap as it is
            super.mapOver(t)
        case defn.FunctionNOf(args, res, contextual) if t.typeSymbol.name.isImpureFunction =>
          if variance > 0 then
            super.mapOver:
              defn.FunctionNOf(args, res, contextual)
                .capturing(Result(mt).singletonCaptureSet)
          else mapOver(t)
        case _ =>
          mapOver(t)
        //.showing(i"mapcap $t = $result")
      override def toString = "toVar"

      object inverse extends BiTypeMap:
        def apply(t: Type) = t match
          case t @ Result(`mt`) =>
            // do a reverse getOrElseUpdate on `seen` to produce the
            // `Fresh` assosicated with `t`
            val it = seen.iterator
            var ref: CaptureRef | Null = null
            while it.hasNext && ref == null do
              val (k, v) = it.next
              if v.annot eq t.annot then ref = k
            if ref == null then
              ref = Fresh()
              seen(ref) = t
            ref
          case _ => mapOver(t)
        def inverse = toVar.this
        override def toString = "toVar.inverse"
    end toVar

    toVar(tp)
  end toResult

  /** Map global roots in function results to result roots */
  def toResultInResults(fail: Message => Unit, keepAliases: Boolean = false)(using Context): TypeMap = new TypeMap with FollowAliasesMap:
    def apply(t: Type): Type = t match
      case defn.RefinedFunctionOf(mt) =>
        val mt1 = apply(mt)
        if mt1 ne mt then mt1.toFunctionType(alwaysDependent = true)
        else t
      case t: MethodType if variance > 0 && t.marksExistentialScope =>
        val t1 = mapOver(t).asInstanceOf[MethodType]
        t1.derivedLambdaType(resType = toResult(t1.resType, t1, fail))
      case CapturingType(parent, refs) =>
        t.derivedCapturingType(this(parent), refs)
      case t: (LazyRef | TypeVar) =>
        mapConserveSuper(t)
      case _ =>
        if keepAliases then mapOver(t) else mapFollowingAliases(t)
  end toResultInResults

  /** If `refs` contains an occurrence of `cap` or `cap.rd`, the current context
   *  with an added property PrintFresh. This addition causes all occurrences of
   *  `Fresh` to be printed as `fresh` instead of `cap`, so that one avoids
   *  confusion in error messages.
   */
  def printContext(refs: (Type | CaptureSet)*)(using Context): Context =
    def hasCap = new TypeAccumulator[Boolean]:
      def apply(x: Boolean, t: Type) =
        x || t.dealiasKeepAnnots.match
          case Fresh(_) => false
          case t: TermRef => t.isCap || this(x, t.widen)
          case x: ThisType => false
          case _ => foldOver(x, t)

    def containsCap(x: Type | CaptureSet): Boolean = x match
      case tp: Type =>
        hasCap(false, tp)
      case refs: CaptureSet =>
        refs.elems.exists(_.stripReadOnly.isCap)

    if refs.exists(containsCap) then ctx.withProperty(PrintFresh, Some(()))
    else ctx
  end printContext
end root