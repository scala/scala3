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
import reporting.Message
import util.{SimpleIdentitySet, EqHashMap}
import ast.tpd
import annotation.constructorOnly
import Capabilities.*

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

  enum Origin:
    case InDecl(sym: Symbol)
    case TypeArg(tp: Type)
    case UnsafeAssumePure
    case Formal(pref: ParamRef, app: tpd.Apply)
    case ResultInstance(methType: Type, meth: Symbol)
    case UnapplyInstance(info: MethodType)
    case NewMutable(tp: Type)
    case NewCapability(tp: Type)
    case LambdaExpected(respt: Type)
    case LambdaActual(restp: Type)
    case OverriddenType(member: Symbol)
    case DeepCS(ref: TypeRef)
    case Unknown

    def explanation(using Context): String = this match
      case InDecl(sym: Symbol) =>
        if sym.is(Method) then i" in the result type of $sym"
        else if sym.exists then i" in the type of $sym"
        else ""
      case TypeArg(tp: Type) =>
        i" of type argument $tp"
      case UnsafeAssumePure =>
        " when instantiating argument of unsafeAssumePure"
      case Formal(pref, app) =>
        val meth = app.symbol
        if meth.exists
        then i" when checking argument to parameter ${pref.paramName} of $meth"
        else ""
      case ResultInstance(mt, meth) =>
        val methDescr = if meth.exists then i"$meth's type " else ""
        i" when instantiating $methDescr$mt"
      case UnapplyInstance(info) =>
        i" when instantiating argument of unapply with type $info"
      case NewMutable(tp) =>
        i" when constructing mutable $tp"
      case NewCapability(tp) =>
        i" when constructing Capability instance $tp"
      case LambdaExpected(respt) =>
        i" when instantiating expected result type $respt of lambda"
      case LambdaActual(restp: Type) =>
        i" when instantiating result type $restp of lambda"
      case OverriddenType(member: Symbol) =>
        i" when instantiating upper bound of member overridden by $member"
      case DeepCS(ref: TypeRef) =>
        i" when computing deep capture set of $ref"
      case Unknown =>
        ""
  end Origin

  /** Map each occurrence of cap to a different Fresh instance
   *  Exception: CapSet^ stays as it is.
   */
  class CapToFresh(origin: Origin)(using Context) extends BiTypeMap, FollowAliasesMap:
    thisMap =>

    override def apply(t: Type) =
      if variance <= 0 then t
      else t match
        case t @ CapturingType(parent: TypeRef, _) if parent.symbol == defn.Caps_CapSet =>
          t
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

    override def mapCapability(c: CaptureRef, deep: Boolean): CaptureRef = c match
      case GlobalCap => FreshCap(origin)
      case _ => super.mapCapability(c, deep)

    override def fuse(next: BiTypeMap)(using Context) = next match
      case next: Inverse => assert(false); Some(IdentityTypeMap)
      case _ => None

    override def toString = "CapToFresh"

    class Inverse extends BiTypeMap, FollowAliasesMap:
      def apply(t: Type): Type = t match
        case t @ CapturingType(_, refs) => mapOver(t)
        case _ => mapFollowingAliases(t)

      override def mapCapability(c: CaptureRef, deep: Boolean): CaptureRef = c match
        case _: FreshCap => GlobalCap
        case _ => super.mapCapability(c, deep)

      def inverse = thisMap
      override def toString = thisMap.toString + ".inverse"

    lazy val inverse = Inverse()

  end CapToFresh

  /** Maps cap to fresh. CapToFresh is a BiTypeMap since we don't want to
   *  freeze a set when it is mapped. On the other hand, we do not want Fresh
   *  values to flow back to cap since that would fail disallowRootCapability
   *  tests elsewhere. We therefore use `withoutMappedFutureElems` to prevent
   *  the map being installed for future use.
   */
  def capToFresh(tp: Type, origin: Origin)(using Context): Type =
    if ccConfig.useSepChecks then
      ccState.withoutMappedFutureElems:
        CapToFresh(origin)(tp)
    else tp

  /** Maps fresh to cap */
  def freshToCap(tp: Type)(using Context): Type =
    if ccConfig.useSepChecks then CapToFresh(Origin.Unknown).inverse(tp) else tp

  /** Map top-level free existential variables one-to-one to Fresh instances */
  def resultToFresh(tp: Type, origin: Origin)(using Context): Type =
    val subst = new TypeMap:
      val seen = EqHashMap[ResultCap, FreshCap | GlobalCap.type]()
      var localBinders: SimpleIdentitySet[MethodType] = SimpleIdentitySet.empty

      def apply(t: Type): Type = t match
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

      override def mapCapability(c: CaptureRef, deep: Boolean) = c match
        case c @ ResultCap(binder) =>
          if localBinders.contains(binder) then c // keep bound references
          else seen.getOrElseUpdate(c, FreshCap(origin)) // map free references to FreshCap
        case _ => super.mapCapability(c, deep)
    end subst

    subst(tp)
  end resultToFresh

  /** Replace all occurrences of `cap` (or fresh) in parts of this type by an existentially bound
   *  variable bound by `mt`.
   *  Stop at function or method types since these have been mapped before.
   */
  def toResult(tp: Type, mt: MethodicType, fail: Message => Unit)(using Context): Type =

    abstract class CapMap extends BiTypeMap:
      override def mapOver(t: Type): Type = t match
        case t @ FunctionOrMethod(args, res) if variance > 0 && !t.isAliasFun =>
          t // `t` should be mapped in this case by a different call to `mapCap`.
        case t: (LazyRef | TypeVar) =>
          mapConserveSuper(t)
        case _ =>
          super.mapOver(t)

    object toVar extends CapMap:
      private val seen = EqHashMap[RootCapability, ResultCap]()

      def apply(t: Type) = t match
        case defn.FunctionNOf(args, res, contextual) if t.typeSymbol.name.isImpureFunction =>
          if variance > 0 then
            super.mapOver:
              defn.FunctionNOf(args, res, contextual)
                .capturing(ResultCap(mt).singletonCaptureSet)
          else mapOver(t)
        case _ =>
          mapOver(t)

      override def mapCapability(c: CaptureRef, deep: Boolean) = c match
        case c: (FreshCap | GlobalCap.type) =>
          if variance > 0 then
            seen.getOrElseUpdate(c, ResultCap(mt))
          else
            if variance == 0 then
              fail(em"""$tp captures the root capability `cap` in invariant position.
                       |This capability cannot be converted to an existential in the result type of a function.""")
            // we accept variance < 0, and leave the cap as it is
            c
        case _ =>
          super.mapCapability(c, deep)

        //.showing(i"mapcap $t = $result")
      override def toString = "toVar"

      object inverse extends BiTypeMap:
        def apply(t: Type) = mapOver(t)

        override def mapCapability(c: CaptureRef, deep: Boolean) = c match
          case c @ ResultCap(`mt`) =>
            // do a reverse getOrElseUpdate on `seen` to produce the
            // `Fresh` assosicated with `t`
            val it = seen.iterator
            var ref: RootCapability | Null = null
            while it.hasNext && ref == null do
              val (k, v) = it.next
              if v eq c then ref = k
            if ref == null then
              ref = FreshCap(Origin.Unknown)
              seen(ref) = c
            ref
          case _ =>
            super.mapCapability(c, deep)

        def inverse = toVar.this
        override def toString = "toVar.inverse"
      end inverse
    end toVar

    toVar(tp)
  end toResult

  /** Map global roots in function results to result roots. Also,
   *  map roots in the types of parameterless def methods.
   */
  def toResultInResults(sym: Symbol, fail: Message => Unit, keepAliases: Boolean = false)(tp: Type)(using Context): Type =
    val m = new TypeMap with FollowAliasesMap:
      def apply(t: Type): Type = t match
        case AnnotatedType(parent @ defn.RefinedFunctionOf(mt), ann) if ann.symbol == defn.InferredDepFunAnnot =>
          val mt1 = mapOver(mt).asInstanceOf[MethodType]
          if mt1 ne mt then mt1.toFunctionType(alwaysDependent = true)
          else parent
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
          try
            if keepAliases then mapOver(t)
            else mapFollowingAliases(t)
          catch case ex: AssertionError =>
            println(i"error while mapping $t")
            throw ex
    m(tp) match
      case tp1: ExprType if sym.is(Method, butNot = Accessor) =>
        tp1.derivedExprType(toResult(tp1.resType, tp1, fail))
      case tp1 => tp1
  end toResultInResults

end root