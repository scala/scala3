package dotty.tools.dotc
package core

import Annotations.Annotation
import Contexts.*
import Flags.*
import StdNames.{nme, tpnme}
import Symbols.*
import Types.*
import Decorators.i
import reporting.*

/** This module defines methods to interpret types originating from sources without explicit nulls
 *  (Java, and Scala code compiled without `-Yexplicit-nulls`) as Scala types with explicit nulls.
 *  In those sources, reference types are implicitly nullable; here we make that nullability explicit.
 *
 *  e.g. given a Java method: `String foo(String arg) { return arg; }`
 *
 *  After calling `nullifyMember`, Scala will see the method as:
 *  `def foo(arg: String | Null): String | Null`
 *
 *  The transformation is (conceptually) a function `n` that adheres to the following rules:
 *    (1) n(T)              = T | Null              if T is a reference type
 *    (2) n(T)              = T                     if T is a value type
 *    (3) n(C[T])           = C[T] | Null           if C is Java-defined
 *    (4) n(C[T])           = C[n(T)] | Null        if C is Scala-defined
 *    (5) n(A|B)            = n(A) | n(B) | Null
 *    (6) n(A&B)            = n(A) & n(B)
 *    (7) n((A1, ..., Am)R) = (n(A1), ..., n(Am))n(R) for a method with arguments (A1, ..., Am) and return type R
 *    (8) n(T)              = T                       otherwise
 *
 *   Treatment of generics (rules 3 and 4):
 *     - if `C` is Java-defined, then `n(C[T]) = C[T] | Null`. That is, we don't recurse
 *       on the type argument, and only add Null on the outside. This is because
 *       `C` itself will be nullified, and in particular so will be usages of `C`'s type argument within C's body.
 *       e.g. calling `get` on a `java.util.List[String]` already returns `String|Null` and not `String`, so
 *       we don't need to write `java.util.List[String | Null]`.
 *     - if `C` is Scala-defined, however, then we want `n(C[T]) = C[n(T)] | Null`. This is because
 *       Scala-defined classes are not implicitly nullified inside their bodies, so we need to indicate that
 *       their type arguments are nullable when the defining source did not use explicit nulls.
 *
 *  Why not use subtyping to nullify “exactly”?
 *  -------------------------------------------------
 *  The symbols we nullify here are often still under construction (e.g. during classfile loading or unpickling),
 *  so we don't always have precise or stable type information available. Using full subtyping checks to determine
 *  which parts are reference types would either force types prematurely or risk cyclic initializations. Therefore,
 *  we use a conservative approach that targets concrete reference types  without depending on precise subtype
 *  information.
 *
 *  Scope and limitations
 *  -------------------------------------------------
 *  The transformation is applied to types attached to members coming from Java and from Scala code compiled without
 *  explicit nulls. The implementation is intentionally conservative and does not attempt to cover the full spectrum
 *  of Scala types. In particular, we do not nullify type parameters or some complex type forms (e.g., match types,
 *  or refined types) beyond straightforward mapping; in such cases we typically recurse only into obviously safe
 *  positions or leave the type unchanged.
 *
 *  Additionally, some kinds of symbols like constructors and enum instances get special treatment.
 *
 *  Null-marked scopes
 *  -------------------------------------------------
 *  Java code in a JSpecify null-marked scope (see `isNullMarked`) declares nullability explicitly:
 *  a reference type is nullable only if it is annotated as such (e.g. `@Nullable String`). In such a
 *  scope, we leave unannotated types unchanged instead of applying `n`. Since references to type
 *  variables are not nullified either, `T` is as nullable as the type argument it is instantiated with.
 */
object ImplicitNullInterop:

  enum NullMode:
    case Flexible // Add flexible types
    case Explicit // Add `| Null`
    case Skip // Don't nullify

  object NullMode:
    def Default(using Context): NullMode = if ctx.flexibleTypes then Flexible else Explicit

  /** Transforms the type `tp` of a member `sym` that originates from a source without explicit nulls.
   *  `tp` is passed explicitly because the type stored in `sym` might not yet be set when this is called.
   */
  def nullifyMember(sym: Symbol, tp: Type, isEnumValueDef: Boolean)(using Context): Type = trace(i"nullifyMember ${sym}, ${tp}"):
    assert(ctx.explicitNulls)

    // Skip `TYPE`, enum values, modules, and special methods like `toString` and `getClass`.
    if isEnumValueDef
      || sym.name == nme.TYPE_
      || sym.name == nme.getClass_
      || sym.name == nme.toString_
      || sym.is(Flags.ModuleVal) then
      return tp

    // In a null-marked scope, unannotated types are non-null: only an explicit nullness
    // annotation makes them nullable.
    val defaultMode = if isNullMarked(sym) then NullMode.Skip else NullMode.Default

    val currentTypeMode =
      // Don't nullify Given/implicit parameters
      if sym.isOneOf(GivenOrImplicitVal) || hasNotNullAnnot(sym) then NullMode.Skip
      else if hasNullableAnnot(sym) then NullMode.Explicit
      else defaultMode

    val resultTypeMode =
      // Don't nullify result type of constructors
      if sym.isConstructor then NullMode.Skip
      else currentTypeMode

    ImplicitNullMap(
      javaDefined = sym.is(JavaDefined),
      defaultMode = defaultMode,
      state = NullMapState(resultTypeMode, currentTypeMode)
    )(tp)

  /** Is the Java-defined member `sym` in a JSpecify null-marked scope?
   *
   *  As specified in https://jspecify.dev/docs/spec/#null-marked-scope, we walk the declarations
   *  enclosing `sym`, starting with `sym` itself. The innermost declaration annotated with
   *  `@NullMarked` or `@NullUnmarked` decides; one annotated with both behaves as if it had neither.
   *  A top-level class compiled by Kotlin is not null-marked unless it says so itself.
   *  The walk stops at the enclosing package, whose annotations are those of `package-info.java`:
   *  packages are not enclosed by their parent packages.
   *
   *  Modules are not supported, since we don't keep track of the module of a Java class.
   */
  private def isNullMarked(sym: Symbol)(using Context): Boolean =
    def loop(owner: Symbol): Boolean =
      if owner.is(PackageClass) then
        nullMarking(owner.info.decl(tpnme.PACKAGE_INFO).symbol).getOrElse(false)
      else
        // Java static members are entered in the companion module class,
        // but the annotations are on the class.
        val decl = if owner.is(ModuleClass) then owner.companionClass else owner
        nullMarking(decl) match
          case Some(marked) => marked
          case None =>
            val isKotlinClass =
              decl.isClass && decl.owner.is(PackageClass) && hasAnnot(decl, defn.KotlinMetadataAnnot)
            !isKotlinClass && loop(owner.owner)
    defn.NullMarkedAnnot.exists && sym.is(JavaDefined) && loop(sym)

  /** `Some(true)` if `decl` is annotated with `@NullMarked`, `Some(false)` if it is annotated with
   *  `@NullUnmarked`, and `None` if it has neither or both annotations.
   */
  private def nullMarking(decl: Symbol)(using Context): Option[Boolean] =
    if !decl.exists then None
    else
      val marked = hasAnnot(decl, defn.NullMarkedAnnot)
      if marked == hasAnnot(decl, defn.NullUnmarkedAnnot) then None
      else Some(marked)

  /** Does `decl` have an annotation of class `cls`? Unlike for the member being nullified, it is
   *  safe to force the enclosing declarations, unless they are being completed.
   */
  private def hasAnnot(decl: Symbol, cls: Symbol)(using Context): Boolean =
    if decl.isCompleting then decl.unforcedAnnotation(cls).isDefined
    else decl.hasAnnotation(cls)

  private def hasNotNullAnnot(sym: Symbol)(using Context): Boolean =
    defn.NotNullAnnots.exists(sym.unforcedAnnotation(_).isDefined)

  private def isNotNullAnnot(annot: Annotation)(using Context): Boolean =
    defn.NotNullAnnots.exists(annot.hasSymbol)

  private def hasNullableAnnot(sym: Symbol)(using Context): Boolean =
    defn.NullableAnnots.exists(sym.unforcedAnnotation(_).isDefined)

  private def isNullableAnnot(annot: Annotation)(using Context): Boolean =
    defn.NullableAnnots.exists(annot.hasSymbol)

  /** Is `annot` a nullness annotation, which is interpreted on types by `nullifyMember`? */
  def isNullnessAnnot(annot: Annotation)(using Context): Boolean =
    isNullableAnnot(annot) || isNotNullAnnot(annot)

  case class NullMapState(
    resultTypeMode: NullMode,
    currentTypeMode: NullMode
  )

  object NullMapState:
    def skipCurrentIf(cond: Boolean, defaultMode: NullMode): NullMapState =
      NullMapState(
        resultTypeMode = defaultMode,
        currentTypeMode = if cond then NullMode.Skip else defaultMode
      )

  /** A type map that implements the nullification function on types. Given a Java-sourced type or a type
   *  coming from Scala code compiled without explicit nulls, this adds `| Null` or `FlexibleType` in the
   *  right places to make nullability explicit in a conservative way (without forcing incomplete symbols).
   *
   *  @param javaDefined  whether the type is from Java source; we always nullify type param refs from Java
   *  @param defaultMode  the mode for positions without nullness annotations: `Skip` in a JSpecify
   *                      null-marked scope, `NullMode.Default` otherwise
   *  @param state        mutable nullification state tracking the current mode for the result type
   *                      (`resultTypeMode`) and the current nesting level (`currentTypeMode`).
   */
  private class ImplicitNullMap(
      val javaDefined: Boolean,
      val defaultMode: NullMode,
      var state: NullMapState
    )(using Context) extends TypeMap:

    /** Should we nullify `tp` at the outermost level?
     *  The symbols are still under construction, so we don't have precise information.
     *  We purposely do not rely on precise subtyping checks here (e.g., asking whether `tp <:< AnyRef`),
     *  because doing so could force incomplete symbols or trigger cycles. Instead, we conservatively
     *  nullify only when we can recognize a concrete reference type or type parameters from Java.
     */
    private def needsNull(tp: Type): Boolean =
      if state.currentTypeMode == NullMode.Skip || !tp.hasSimpleKind then false
      else tp.dealias match
        case tp: TypeRef =>
          // We don't modify value types because they're non-nullable even in Java.
          val isValueOrSpecialClass =
            tp.symbol.isValueClass
            || tp.isRef(defn.NullClass)
            || tp.isRef(defn.NothingClass)
            || tp.isRef(defn.UnitClass)
            || tp.isRef(defn.SingletonClass)
            || tp.isRef(defn.AnyKindClass)
            || tp.isRef(defn.AnyClass)
          !isValueOrSpecialClass && (javaDefined || tp.symbol.isNullableClassAfterErasure)
        case tp: TypeParamRef =>
          javaDefined
        case tp: AppliedType =>
          // We don't nullify varargs (repeated parameters) or tuple type at the top level.
          // Example: if `setNames` is a Java method with signature `void setNames(String... names)`,
          // then its Scala signature will be `def setNames(names: (String|Null)*): Unit`.
          // This is because `setNames(null)` passes as argument a single-element array containing the value `null`,
          // and not a `null` array. Only a flexible type can wrap a repeated parameter type, even if
          // the array is annotated as nullable.
          tp.tycon match
            case tycon: TypeRef =>
              val isRepeatedParam = tycon.isRef(defn.RepeatedParamClass)
              !(isRepeatedParam && state.currentTypeMode != NullMode.Flexible || defn.isTupleClass(tycon.symbol))
            case _ => true
        case _ => false

    def nullify(tp: Type): Type =
      if !needsNull(tp) then tp
      else if state.currentTypeMode == NullMode.Flexible then FlexibleType.make(tp)
      else OrNull(tp)

    override def apply(tp: Type): Type = tp match
      case tp: TypeRef =>
        nullify(tp)
      case tp: TypeParamRef =>
        nullify(tp)
      case tp: AnnotatedType =>
        val savedState = state
        var isNullAnnot = true
        val underlyingMode =
          if isNullableAnnot(tp.annot) then NullMode.Explicit
          else if isNotNullAnnot(tp.annot) then NullMode.Skip
          else
            isNullAnnot = false
            state.currentTypeMode
        state = savedState.copy(currentTypeMode = underlyingMode)
        val parent2 = this(tp.parent)
        state = savedState
        if isNullAnnot then parent2
        else parent2 match
          case FlexibleType(parent2a) =>
            FlexibleType(derivedAnnotatedType(tp, parent2a, tp.annot))
          case OrNull(parent2a) =>
            OrNull(derivedAnnotatedType(tp, parent2a, tp.annot))
          case _ =>
            derivedAnnotatedType(tp, parent2, tp.annot)
      case FlexibleType(_) =>
        // A flexible type is already a fully-nullified result, so nullification must be
        // idempotent on it: leave it unchanged. Without this case it would match the
        // `AppliedType` case below (a flexible type is encoded as `AppliedType(FlexibleType, T)`)
        // and get wrapped a second time, e.g. `(Array[T])?` becoming `((Array[T])?)?`.
        tp
      case appTp @ AppliedType(tycon, targs) =>
        val savedState = state
        // If Java-defined tycon, don't nullify outer level of type args (Java classes are fully nullified)
        state = NullMapState.skipCurrentIf(tp.classSymbol.is(JavaDefined), defaultMode)
        val targs2 = targs.map(this)
        state = savedState

        val appTp2 = derivedAppliedType(appTp, tycon, targs2)
        nullify(appTp2)
      case ptp: PolyType =>
        derivedLambdaType(ptp)(ptp.paramInfos, this(ptp.resType))
      case mtp: MethodType =>
        val savedState = state

        // Don't nullify param types for implicit/using sections
        state = NullMapState.skipCurrentIf(mtp.isImplicitMethod, defaultMode)
        val paramInfos2 = mtp.paramInfos.map(this)

        state = NullMapState(
          resultTypeMode = defaultMode,
          currentTypeMode = savedState.resultTypeMode
        )
        val resType2 = this(mtp.resType)

        state = savedState
        derivedLambdaType(mtp)(paramInfos2, resType2)
      case tp: ExprType =>
        mapOver(tp)
      case tp: TypeAlias =>
        mapOver(tp)
      case tp: TypeBounds =>
        mapOver(tp)
      case tp: AndOrType =>
        // For unions/intersections we recurse into both sides.
        // If both sides are nullable, we only add `| Null` once.
        // This keeps the result minimal and avoids duplicating `| Null`
        // on both sides and at the outer level.
        (this(tp.tp1), this(tp.tp2)) match
          case (FlexibleType(t1), FlexibleType(t2)) if ctx.flexibleTypes =>
            FlexibleType(derivedAndOrType(tp, t1, t2))
          case (OrNull(t1), OrNull(t2)) =>
            OrNull(derivedAndOrType(tp, t1, t2))
          case (t1, t2) =>
            derivedAndOrType(tp, t1, t2)
      case tp: RefinedType =>
        val parent2 = this(tp.parent)

        val savedState = state
        // We skip nullification for the refined info for simplicity.
        state = NullMapState(
          resultTypeMode = NullMode.Skip,
          currentTypeMode = NullMode.Skip
        )
        val refinedInfo2 = this(tp.refinedInfo)
        state = savedState

        // If the parent type becomes nullable, then we pop the nullification to the outer level.
        parent2 match
          case FlexibleType(parent2a) =>
            FlexibleType(derivedRefinedType(tp, parent2a, refinedInfo2))
          case OrNull(parent2a) =>
            OrNull(derivedRefinedType(tp, parent2a, refinedInfo2))
          case _ =>
            derivedRefinedType(tp, parent2, refinedInfo2)
      case _ =>
        // In all other cases, return the type unchanged.
        // In particular, if the type is a ConstantType, then we don't nullify it because it is the
        // type of a final non-nullable field. We also deliberately do not attempt to nullify
        // complex computed types such as match types here; those remain as-is to avoid forcing
        // incomplete information during symbol construction.
        tp
    end apply
  end ImplicitNullMap