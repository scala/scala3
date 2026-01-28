package dotty.tools
package dotc
package cc

import core.*
import Symbols.*, Types.*, Flags.*, Contexts.*, Names.*, Decorators.*
import Capabilities.*
import util.SrcPos
import config.Printers.capt
import config.Feature
import ast.tpd.Tree
import typer.ProtoTypes.LhsProto
import StdNames.nme

/** Handling mutability and read-only access
 */
object Mutability:

  /** Either OK, or a reason why capture set cannot be exclusive */
  private enum Exclusivity derives CanEqual:
    case OK

    /** Enclosing symbol `sym` is a method of class `cls`, but not an update method */
    case NotInUpdateMethod(sym: Symbol, cls: Symbol)

    /** Access to `this` from outside its class (not sure this can happen) */
    case OutsideClass(cls: Symbol)

    /** A prefix type `tp` has a read-only capture set */
    case ReadOnly(tp: Type)

    def isOK: Boolean = this == OK

    def andAlso(that: Exclusivity): Exclusivity =
      if this == OK then that else this

    /** A textual description why `qualType` is not exclusive */
    def description(qualType: Type)(using Context): String = this.runtimeChecked match
      case Exclusivity.ReadOnly(tp) =>
        if qualType eq tp then
          i"its capture set ${qualType.captureSet} is read-only"
        else
          i"the capture set ${tp.captureSet} of its prefix $tp is read-only"
      case Exclusivity.NotInUpdateMethod(sym: Symbol, cls: Symbol) =>
        i"the access is in $sym, which is not an update method"
      case Exclusivity.OutsideClass(cls: Symbol) =>
        i"the access from is from ouside $cls"

  end Exclusivity

  extension (sym: Symbol)
    /** An update method is either a method marked with `update` or a setter
     *  of a field of a Stateful class that's not annotated with @uncheckedCaptures.
     *  `update` is implicit for `consume` methods of Stateful classes.
     */
    def isUpdateMethod(using Context): Boolean =
      sym.isAllOf(Mutable | Method)
        && (!sym.is(Accessor) || (sym.isSetter && sym.owner.derivesFrom(defn.Caps_Stateful) && !sym.field.hasAnnotation(defn.UntrackedCapturesAnnot)))
      || ccConfig.strictMutability && sym.name == nme.update && sym == defn.Array_update

    /** A read-only member is a lazy val or a method that is not an update method. */
    def isReadOnlyMember(using Context): Boolean =
      sym.isOneOf(MethodOrLazy) && !sym.isUpdateMethod

    private def inExclusivePartOf(cls: Symbol)(using Context): Exclusivity =
      import Exclusivity.*
      if sym == cls then OK // we are directly in `cls` or in one of its constructors
      else if sym.isUpdateMethod then OK
      else if sym.owner == cls then
        if sym.isConstructor || !sym.isOneOf(MethodOrLazy) then OK
        else NotInUpdateMethod(sym, cls)
      else if sym.isRoot then OutsideClass(cls)
      else sym.owner.inExclusivePartOf(cls)

  extension (tp: Type)
    /** Is this a type extending `Stateful` that has non-private update methods
     *  or mutable fields?
     */
    def isStatefulType(using Context): Boolean =
      tp.derivesFrom(defn.Caps_Stateful)
      && tp.membersBasedOnFlags(Mutable, EmptyFlags).exists: mbr =>
        if mbr.symbol.is(Method) then mbr.symbol.isUpdateMethod
        else !mbr.symbol.hasAnnotation(defn.UntrackedCapturesAnnot)

    /** OK, except if `tp` extends `Stateful` but `tp`'s capture set is non-exclusive
     *  @param required  if true, exclusivity can be obtained by setting the mutability
     *                   status of some capture set variable from Ignored to Writer.
     */
    private def exclusivity(required: Boolean)(using Context): Exclusivity =
      if tp.derivesFrom(defn.Caps_Stateful) then
        tp match
          case tp: Capability if tp.isExclusive(required) => Exclusivity.OK
          case _ =>
            if tp.captureSet.isExclusive(required) then Exclusivity.OK
            else Exclusivity.ReadOnly(tp)
      else Exclusivity.OK

    /** Test conditions (1) and (2) listed in `adaptReadOnly` below
     *  @param required  if true, exclusivity can be obtained by setting the mutability
     *                   status of some capture set variable from Ignored to Writer.
     */
    private def exclusivityInContext(required: Boolean = false)(using Context): Exclusivity = tp match
      case tp: ThisType =>
        if tp.derivesFrom(defn.Caps_Stateful)
        then ctx.owner.inExclusivePartOf(tp.cls)
        else Exclusivity.OK
      case tp @ TermRef(prefix: Capability, _) =>
        prefix.exclusivityInContext(required).andAlso(tp.exclusivity(required))
      case _ =>
        tp.exclusivity(required)

    def expectsReadOnly(using Context): Boolean = tp match
      case tp: PathSelectionProto =>
        tp.selector.isReadOnlyMember || tp.selector.isMutableVar && tp.pt != LhsProto
      case _ =>
        tp.isValueType
        && (!tp.isStatefulType || tp.captureSet.mutability == CaptureSet.Mutability.Reader)

  extension (ref: TermRef | ThisType)
    /** Map `ref` to `ref.readOnly` if its type extends Mutable, and one of the
     *  following is true:
     *    - it appears in a non-exclusive context,
     *    - the expected type is a value type that is not a stateful type,
     *    - the expected type is a read-only selection
     */
    def adjustReadOnly(pt: Type)(using Context): Capability = {
      if ref.derivesFromStateful
          && (pt.expectsReadOnly || ref.exclusivityInContext() != Exclusivity.OK)
      then ref.readOnly
      else ref
    }.showing(i"Adjust RO $ref vs $pt = $result", capt)

  /** Check that we can call an update method of `qualType` or perform an assignment
   *  of a field of `qualType`.
   */
  def checkUpdate(qualType: Type, pos: SrcPos)(msg: => String)(using Context): Unit =
    qualType.exclusivityInContext(required = true) match
      case Exclusivity.OK =>
      case err =>
        report.error(em"$msg\nsince ${err.description(qualType)}.", pos)

  /** Perform step (3) of adaptReadOnly below.
   *
   *  If actual is a capturing type T^C extending Stateful, and expected is an
   *  unboxed non-singleton value type not extending mutable, widen the capture
   *  set `C` to `ro(C).reader`.
   *  The unboxed condition ensures that the expected type is not a type variable
   *  that's upper bounded by a read-only type. In this case it would not be sound
   *  to widen to the read-only set, since that set can be propagated
   *  by the type variable instantiation.
   */
  private def adaptReadOnlyToExpected(actual: Type, expected: Type)(using Context): Type = reporting.trace(i"improv ro $actual vs $expected"):
    actual.dealiasKeepAnnots match
    case actual @ CapturingType(parent, refs) =>
      val parent1 = adaptReadOnlyToExpected(parent, expected)
      val refs1 =
        if parent1.derivesFrom(defn.Caps_Stateful)
            && expected.isValueType
            && (!expected.derivesFromStateful || expected.captureSet.isAlwaysReadOnly)
            && !expected.isSingleton
        then refs.readOnly
        else refs
      actual.derivedCapturingType(parent1, refs1)
    case actual @ FunctionOrMethod(aargs, ares) =>
      expected.dealias.stripCapturing match
        case FunctionOrMethod(eargs, eres) =>
          actual.derivedFunctionOrMethod(aargs, adaptReadOnlyToExpected(ares, eres))
        case _ =>
          actual
    case actual @ AppliedType(atycon, aargs) =>
      def improveArgs(aargs: List[Type], eargs: List[Type], formals: List[ParamInfo]): List[Type] =
        aargs match
          case aargs @ (aarg :: aargs1) =>
            val aarg1 =
              if formals.head.paramVariance.is(Covariant)
              then adaptReadOnlyToExpected(aarg, eargs.head)
              else aarg
            aargs.derivedCons(aarg1, improveArgs(aargs1, eargs.tail, formals.tail))
          case Nil =>
            aargs
      val expected1 = expected.dealias.stripCapturing
      val esym = expected1.typeSymbol
      expected1 match
        case AppliedType(etycon, eargs) =>
          if atycon.typeSymbol == esym then
            actual.derivedAppliedType(atycon,
              improveArgs(aargs, eargs, etycon.typeParams))
          else if esym.isClass then
            // This case is tricky: Try to lift actual to the base type with class `esym`,
            // improve the resulting arguments, and figure out if anything can be
            // deduced from that for the original arguments.
            actual.baseType(esym) match
              case base @ AppliedType(_, bargs) =>
                // If any of the base type arguments can be improved, check
                // whether they are the same as an original argument, and in this
                // case improve the original argument.
                val iargs = improveArgs(bargs, eargs, etycon.typeParams)
                if iargs ne bargs then
                  val updates =
                    for
                      (barg, iarg) <- bargs.lazyZip(iargs)
                      if barg ne iarg
                      aarg <- aargs.find(_ eq barg)
                    yield (aarg, iarg)
                  if updates.nonEmpty then AppliedType(atycon, aargs.map(updates.toMap))
                  else actual
                else actual
              case _ => actual
          else actual
        case _ =>
          actual
    case actual @ RefinedType(aparent, aname, ainfo) =>
      expected.dealias.stripCapturing match
        case RefinedType(eparent, ename, einfo) if aname == ename =>
          actual.derivedRefinedType(
            adaptReadOnlyToExpected(aparent, eparent),
            aname,
            adaptReadOnlyToExpected(ainfo, einfo))
        case _ =>
          actual
    case actual @ AnnotatedType(parent, ann) =>
      actual.derivedAnnotatedType(adaptReadOnlyToExpected(parent, expected), ann)
    case _ =>
      actual
  end adaptReadOnlyToExpected

  /** Adapt type `actual` so that it represents a read-only access
   *  if needed. `actual` is the widened version of original with capture
   *  set improved by the VAR rule. One of the following conditions must hold for
   *  adaptions to be applied (see modularity.md, section "Read-Only Accesses" for context):
   *   1. The `original` reference is a `this` of a type extending Stateful and
   *      the access is not from an update method of the class of `this`.
   *   2. The `original` reference refers to a type extending Stateful and is a path
   *      where a prefix of that path has a read-only capture set.
   *   3. The expected type corresponding to some part of `actual` that refers
   *      to a type extending Stateful is a value type that is not a stateful type.
   *      In that case this part is adapted to a read-only capture set.
   */
  def adaptReadOnly(actual: Type, original: Type, expected: Type, tree: Tree)(using Context): Type =
    adaptReadOnlyToExpected(actual, expected) match
      case improved @ CapturingType(parent, refs)
      if parent.derivesFrom(defn.Caps_Stateful)
          && expected.isValueType
          && refs.isExclusive()
          && !original.exclusivityInContext().isOK =>
        improved.derivedCapturingType(parent, refs.readOnly)
          .showing(i"Adapted readonly $improved for $tree with original = $original in ${ctx.owner} --> $result", capt)
      case improved =>
        improved

  /** Apply `caps.freeze(...)`. Strip all capture sets of covariant Mutable
   *  types, turning them into `CaptureSet.emptyOfMutable`. Only Mutable types
   *  that contribute to the overall capture set are considered, since that is the
   *  set analyzed by consume/use checking. That means that double-flip covariant
   *  and boxed capture sets are not dropped.
   */
  def freeze(tp: Type, pos: SrcPos)(using Context): Type = tp.widen match
    case tpw @ CapturingType(parent, refs)
    if parent.derivesFromMutable && !tpw.isBoxed =>
      if !Feature.enabled(Feature.separationChecking) then
        report.warning(
          em"""freeze is safe only if separation checking is enabled.
              |You can enable separation checking with the language import
              |
              |   import language.experimental.separationChecking""",
          pos)
      tpw.derivedCapturingType(parent, CaptureSet.emptyOfStateful)
    case _ => tp
end Mutability