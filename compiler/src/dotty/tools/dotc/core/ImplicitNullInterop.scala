package dotty.tools.dotc
package core

import Contexts.*
import Flags.*
import StdNames.nme
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
 */
object ImplicitNullInterop:

  /** Transforms the type `tp` of a member `sym` that originates from a source without explicit nulls.
   *  `tp` is passed explicitly because the type stored in `sym` might not yet be set when this is called.
   */
  def nullifyMember(sym: Symbol, tp: Type, isEnumValueDef: Boolean)(using Context): Type = trace(i"nullifyMember ${sym}, ${tp}"):
    assert(ctx.explicitNulls)

    // Skip `TYPE`, enum values, and modules
    if isEnumValueDef || sym.name == nme.TYPE_ || sym.is(Flags.ModuleVal) then
      return tp

    // Don't nullify result type for `toString`, constructors, and @NotNull methods
    val skipResultType = sym.name == nme.toString_ || sym.isConstructor || hasNotNullAnnot(sym)
    // Don't nullify Given/implicit parameters
    val skipCurrentLevel = sym.isOneOf(GivenOrImplicitVal)

    val map = new ImplicitNullMap(
      javaDefined = sym.is(JavaDefined),
      skipResultType = skipResultType,
      skipCurrentLevel = skipCurrentLevel)
    map(tp)

  private def hasNotNullAnnot(sym: Symbol)(using Context): Boolean =
    ctx.definitions.NotNullAnnots.exists(nna => sym.unforcedAnnotation(nna).isDefined)

  /** A type map that implements the nullification function on types. Given a Java-sourced type or a type
   *  coming from Scala code compiled without explicit nulls, this adds `| Null` or `FlexibleType` in the
   *  right places to make nullability explicit in a conservative way (without forcing incomplete symbols).
   *
   *  @param javaDefined        whether the type is from Java source, we always nullify type param refs from Java
   *  @param skipResultType     do not nullify the method result type at the outermost level (e.g. for `toString`,
   *                            constructors, or methods annotated as not-null)
   *  @param skipCurrentLevel   do not nullify at the current level (used for implicit/Given parameters, varargs, etc.)
   */
  private class ImplicitNullMap(
      val javaDefined: Boolean,
      var skipResultType: Boolean = false,
      var skipCurrentLevel: Boolean = false
    )(using Context) extends TypeMap:

    def nullify(tp: Type): Type = if ctx.flexibleTypes then FlexibleType(tp) else OrNull(tp)

    /** Should we nullify `tp` at the outermost level?
     *  The symbols are still under construction, so we don't have precise information.
     *  We purposely do not rely on precise subtyping checks here (e.g., asking whether `tp <:< AnyRef`),
     *  because doing so could force incomplete symbols or trigger cycles. Instead, we conservatively
     *  nullify only when we can recognize a concrete reference type shape.
     */
    def needsNull(tp: Type): Boolean =
      if skipCurrentLevel || !tp.hasSimpleKind then false
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
        case _ => false

    // We don't nullify varargs (repeated parameters) at the top level.
    // Example: if `setNames` is a Java method with signature `void setNames(String... names)`,
    // then its Scala signature will be `def setNames(names: (String|Null)*): Unit`.
    // This is because `setNames(null)` passes as argument a single-element array containing the value `null`,
    // and not a `null` array.
    def tyconNeedsNull(tp: Type): Boolean =
      if skipCurrentLevel then false
      else tp match
        case tp: TypeRef
          if !ctx.flexibleTypes && tp.isRef(defn.RepeatedParamClass) => false
        case _ => true

    override def apply(tp: Type): Type = tp match
      case tp: TypeRef if needsNull(tp) =>
        nullify(tp)
      case tp: TypeParamRef if needsNull(tp) =>
        nullify(tp)
      case appTp @ AppliedType(tycon, targs) =>
        val savedSkipCurrentLevel = skipCurrentLevel

        // If Java-defined tycon, don't nullify outer level of type args (Java classes are fully nullified)
        skipCurrentLevel = tp.classSymbol.is(JavaDefined)
        val targs2 = targs.map(this)

        skipCurrentLevel = savedSkipCurrentLevel
        val appTp2 = derivedAppliedType(appTp, tycon, targs2)
        if tyconNeedsNull(tycon) then nullify(appTp2) else appTp2
      case ptp: PolyType =>
        derivedLambdaType(ptp)(ptp.paramInfos, this(ptp.resType))
      case mtp: MethodType =>
        val savedSkipCurrentLevel = skipCurrentLevel

        // Don't nullify param types for implicit/using sections
        skipCurrentLevel = mtp.isImplicitMethod
        val paramInfos2 = mtp.paramInfos.map(this)

        skipCurrentLevel = skipResultType
        val resType2 = this(mtp.resType)

        skipCurrentLevel = savedSkipCurrentLevel
        derivedLambdaType(mtp)(paramInfos2, resType2)
      case tp: TypeAlias =>
        mapOver(tp)
      case tp: TypeBounds =>
        mapOver(tp)
      case tp: AndOrType =>
        // For unions/intersections we recurse into constituents but do not force an outer `| Null` here;
        // outer nullability is handled by the surrounding context. This keeps the result minimal and avoids
        // duplicating `| Null` on both sides and at the outer level.
        mapOver(tp)
      case tp: ExprType =>
        mapOver(tp)
      case tp: AnnotatedType =>
        // We don't nullify the annotation part.
        derivedAnnotatedType(tp, this(tp.underlying), tp.annot)
      case tp: RefinedType =>
        val savedSkipCurrentLevel = skipCurrentLevel

        // Nullify parent at outer level; not refined members
        skipCurrentLevel = true
        val parent2 = this(tp.parent)

        skipCurrentLevel = false
        val refinedInfo2 = this(tp.refinedInfo)

        skipCurrentLevel = savedSkipCurrentLevel
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