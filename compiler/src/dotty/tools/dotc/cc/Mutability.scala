package dotty.tools
package dotc
package cc

import core.*
import Symbols.*, Types.*, Flags.*, Contexts.*, Names.*, Decorators.*
import Capabilities.*
import util.SrcPos
import config.Printers.capt
import ast.tpd.Tree

/** Handling mutability and read-only access
 */
object Mutability:

  /** Either OK, or a reason why capture set cannot be exclusive */
  private enum Exclusivity:
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
     *  of a field of a Mutable class that's not annotated with @uncheckedCaptures.
     *  `update` is implicit for `consume` methods of Mutable classes.
     */
    def isUpdateMethod(using Context): Boolean =
      sym.isAllOf(Mutable | Method)
        && (if sym.isSetter then
              sym.owner.derivesFrom(defn.Caps_Mutable)
              && !sym.field.hasAnnotation(defn.UntrackedCapturesAnnot)
            else true
           )

    /** A read-only method is a real method (not an accessor) in a type extending
     *  Mutable that is not an update method. Included are also lazy vals in such types.
     */
    def isReadOnlyMethodOrLazyVal(using Context): Boolean =
      sym.isOneOf(MethodOrLazy, butNot = Mutable | Accessor)
      && sym.owner.derivesFrom(defn.Caps_Mutable)

    private def inExclusivePartOf(cls: Symbol)(using Context): Exclusivity =
      import Exclusivity.*
      if sym == cls then OK // we are directly in `cls` or in one of its constructors
      else if sym.isUpdateMethod then OK
      else if sym.owner == cls then
        if sym.isConstructor then OK
        else NotInUpdateMethod(sym, cls)
      else if sym.isStatic then OutsideClass(cls)
      else sym.owner.inExclusivePartOf(cls)

  extension (tp: Type)
    /** Is this a type extending `Mutable` that has non-private update methods
     *  or mutable fields?
     */
    def isMutableType(using Context): Boolean =
      tp.derivesFrom(defn.Caps_Mutable)
      && tp.membersBasedOnFlags(Mutable, EmptyFlags).exists: mbr =>
        if mbr.symbol.is(Method) then mbr.symbol.isUpdateMethod
        else !mbr.symbol.hasAnnotation(defn.UntrackedCapturesAnnot)

    /** OK, except if `tp` extends `Mutable` but `tp`'s capture set is non-exclusive */
    private def exclusivity(using Context): Exclusivity =
      if tp.derivesFrom(defn.Caps_Mutable) then
        tp match
          case tp: Capability if tp.isExclusive => Exclusivity.OK
          case _ => tp.captureSet.exclusivity(tp)
      else Exclusivity.OK

    /** Test conditions (1) and (2) listed in `adaptReadOnly` below */
    private def exclusivityInContext(using Context): Exclusivity = tp match
      case tp: ThisType =>
        if tp.derivesFrom(defn.Caps_Mutable)
        then ctx.owner.inExclusivePartOf(tp.cls)
        else Exclusivity.OK
      case tp @ TermRef(prefix: Capability, _) =>
        prefix.exclusivityInContext.andAlso(tp.exclusivity)
      case _ =>
        tp.exclusivity

  extension (cs: CaptureSet)
    private def exclusivity(tp: Type)(using Context): Exclusivity =
      if cs.isExclusive then Exclusivity.OK else Exclusivity.ReadOnly(tp)

  extension (ref: TermRef | ThisType)
    /** Map `ref` to `ref.readOnly` if its type extends Mutble, and one of the
     *  following is true: it appears in a non-exclusive context, or the expected
     *  type is a value type that is not a mutable type.
     */
    def adjustReadOnly(pt: Type)(using Context): Capability =
      if ref.derivesFromMutable
          && (pt.isValueType && !pt.isMutableType
              || ref.exclusivityInContext != Exclusivity.OK)
      then ref.readOnly
      else ref

  /** Check that we can call an update method of `qualType` or perform an assignment
   *  of a field of `qualType`.
   */
  def checkUpdate(qualType: Type, pos: SrcPos)(msg: => String)(using Context): Unit =
    qualType.exclusivityInContext match
      case Exclusivity.OK =>
      case err =>
        report.error(em"$msg\nsince ${err.description(qualType)}.", pos)

  /** Perform step (3) of adaptReadOnly below.
   *
   *  If actual is a capturing type T^C extending Mutable, and expected is an
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
        if parent1.derivesFrom(defn.Caps_Mutable)
            && expected.isValueType
            && (!expected.derivesFromMutable || expected.captureSet.isAlwaysReadOnly)
            && !expected.isSingleton
            && actual.isBoxedCapturing == expected.isBoxedCapturing
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
   *  set improved by the VAR rule. The conditions for adaptation are
   *  as follows (see modularity.md, section "Read-Only Accesses" for context)
   *   1. The `original` reference is a `this` of a type extending Mutable and
   *      the access is not from an update method of the class of `this`.
   *   2. The `original` reference refers to a type extending Mutable and is a path
   *      where a prefix of that path has a read-only capture set.
   *   3. The expected type corresponding to some part of `actual` that refers
   *      to a type extending Mutable is a value type that is not a mutable type.
   *      In that case this part is adapted to a read-only capture set.
   */
  def adaptReadOnly(actual: Type, original: Type, expected: Type, tree: Tree)(using Context): Type =
    adaptReadOnlyToExpected(actual, expected) match
      case improved @ CapturingType(parent, refs)
      if parent.derivesFrom(defn.Caps_Mutable)
          && expected.isValueType
          && refs.isExclusive
          && !original.exclusivityInContext.isOK =>
        improved.derivedCapturingType(parent, refs.readOnly)
          .showing(i"Adapted readonly $improved for $tree with original = $original in ${ctx.owner} --> $result", capt)
      case improved =>
        improved

end Mutability