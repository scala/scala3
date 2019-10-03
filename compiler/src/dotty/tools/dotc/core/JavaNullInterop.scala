package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.JavaDefined
import dotty.tools.dotc.core.StdNames.{jnme, nme}
import dotty.tools.dotc.core.Symbols.{Symbol, defn, _}
import dotty.tools.dotc.core.Types.{AndType, AppliedType, LambdaType, MethodType, OrType, PolyType, Type, TypeAlias, TypeMap, TypeParamRef, TypeRef}
import NullOpsDecorator._

/** This module defines methods to interpret types of Java symbols, which are implicitly nullable in Java,
 *  as Scala types, which are explicitly nullable.
 *
 *  The transformation is (conceptually) a function `n` that adheres to the following rules:
 *    (1) n(T)              = T|JavaNull              if T is a reference type
 *    (2) n(T)              = T                       if T is a value type
 *    (3) n(C[T])           = C[T]|JavaNull           if C is Java-defined
 *    (4) n(C[T])           = C[n(T)]|JavaNull        if C is Scala-defined
 *    (5) n(A|B)            = n(A)|n(B)|JavaNull
 *    (6) n(A&B)            = n(A) & n(B)
 *    (7) n((A1, ..., Am)R) = (n(A1), ..., n(Am))n(R) for a method with arguments (A1, ..., Am) and return type R
 *    (8) n(T)              = T                       otherwise
 *
 *   Treatment of generics (rules 3 and 4):
 *     - if `C` is Java-defined, then `n(C[T]) = C[T]|JavaNull`. That is, we don't recurse
 *       on the type argument, and only add JavaNull on the outside. This is because
 *       `C` itself will be nullified, and in particular so will be usages of `C`'s type argument within C's body.
 *       e.g. calling `get` on a `java.util.List[String]` already returns `String|Null` and not `String`, so
 *       we don't need to write `java.util.List[String|Null]`.
 *     - if `C` is Scala-defined, however, then we want `n(C[T]) = C[n(T)]|JavaNull`. This is because
 *       `C` won't be nullified, so we need to indicate that its type argument is nullable.
 *
 *   Notice that since the transformation is only applied to types attached to Java symbols, it doesn't need
 *   to handle the full spectrum of Scala types. Additionally, some kinds of symbols like constructors and
 *   enum instances get special treatment.
 */
object JavaNullInterop {

  /** Transforms the type `tp` of Java member `sym` to be explicitly nullable.
   *  `tp` is needed because the type inside `sym` might not be set when this method is called.
   *
   *  e.g. given a Java method
   *  String foo(String arg) { return arg; }
   *
   *  After calling `nullifyMember`, Scala will see the method as
   *
   *  def foo(arg: String|JavaNull): String|JavaNull
   *
   *  This nullability function uses `JavaNull` instead of vanilla `Null`, for usability.
   *  This means that we can select on the return of `foo`:
   *
   *  val len = foo("hello").length
   *
   *  But the selection can throw an NPE if the returned value is `null`.
   */
  def nullifyMember(sym: Symbol, tp: Type)(implicit ctx: Context): Type = {
    assert(ctx.explicitNulls)
    assert(sym.is(JavaDefined), "can only nullify java-defined members")

    // Some special cases when nullifying the type
    if (sym.name == nme.TYPE_ || sym.isAllOf(Flags.JavaEnumValue))
      // Don't nullify the `TYPE` field in every class and Java enum instances
      tp
    else if (sym.name == nme.toString_ || sym.isConstructor)
      // Don't nullify the return type of the `toString` method and constructors
      nullifyParamsOnly(tp)
    else
      // Otherwise, nullify everything
      nullifyType(tp)
  }

  /** A Nullifier for handling a method or poly.
   *  @param nnParams the indices of the method parameters that should be considered
   *    "non-null" (should not be nullified).
   *  @param nnRes whether the result type should be nullified.
   *
   *  For the purposes of both `nnParams` and `nnRes`, when a parameter or return type is not nullified,
   *  this applies only at the top level. e.g. suppose we have a Java result type `Array[String]` and `nnRes` is set.
   *  Scala will see `Array[String|JavaNull]`; the array element type is still nullified.
   */
  private case class MethodNullifier(nnParams: Seq[Int], nnRes: Boolean)(implicit ctx: Context) extends TypeMap {

    private def spare(tp: Type): Type = {
      nullifyType(tp).stripNull
    }

    override def apply(tp: Type): Type = {
      tp match {
        case ptp: PolyType =>
          derivedLambdaType(ptp)(ptp.paramInfos, this(ptp.resType))
        case mtp: MethodType =>
          val paramTpes = mtp.paramInfos.zipWithIndex.map {
            case (paramInfo, index) =>
              // TODO(abeln): the sequence lookup can be optimized, because the indices
              // in it appear in increasing order.
              if (nnParams.contains(index)) spare(paramInfo) else nullifyType(paramInfo)
          }
          val resTpe = if (nnRes) spare(mtp.resType) else nullifyType(mtp.resType)
          derivedLambdaType(mtp)(paramTpes, resTpe)
      }
    }
  }

  /** Only nullify method parameters (but not result types). */
  private def nullifyParamsOnly(tp: Type)(implicit ctx: Context): Type =
    MethodNullifier(nnParams = Seq.empty, nnRes = true)(ctx)(tp)

  /** Nullifies a Java type by adding `| JavaNull` in the relevant places. */
  private def nullifyType(tpe: Type)(implicit ctx: Context): Type = {
    val nullMap = new JavaNullMap(alreadyNullable = false)
    nullMap(tpe)
  }

  /** A type map that adds `| JavaNull`.
   *  @param alreadyNullable whether the type being mapped is already nullable (at the outermost level).
   *                         This is needed so that `JavaNullMap(A | B)` gives back `(A | B) | JavaNull`,
   *                         instead of `(A|JavaNull | B|JavaNull) | JavaNull`.
   */
  private class JavaNullMap(var alreadyNullable: Boolean)(implicit ctx: Context) extends TypeMap {
    /** Should we nullify `tp` at the outermost level? */
    def needsTopLevelNull(tp: Type): Boolean = {
      !alreadyNullable && (tp match {
        case tp: TypeRef =>
          // We don't modify value types because they're non-nullable even in Java.
          !tp.symbol.isValueClass &&
          // We don't modify `Any` because it's already nullable.
          !tp.isRef(defn.AnyClass) &&
          // We don't nullify Java varargs at the top level.
          // Example: if `setNames` is a Java method with signature `void setNames(String... names)`,
          // then its Scala signature will be `def setNames(names: (String|JavaNull)*): Unit`.
          // This is because `setNames(null)` passes as argument a single-element array containing the value `null`,
          // and not a `null` array.
          !tp.isRef(defn.RepeatedParamClass)
        case _ => true
      })
    }

    /** Should we nullify the type arguments to the given generic `tp`?
     *  We only nullify the inside of Scala-defined generics.
     *  This is because Java classes are _all_ nullified, so both `java.util.List[String]` and
     *  `java.util.List[String|Null]` contain nullable elements.
     */
    def needsNullArgs(tp: AppliedType): Boolean = {
      !tp.classSymbol.is(JavaDefined)
    }

    override def apply(tp: Type): Type = {
      tp match {
        case tp: TypeRef if needsTopLevelNull(tp) => tp.toJavaNullableUnion
        case appTp @ AppliedType(tycons, targs) =>
          val targs2 = if (needsNullArgs(appTp)) targs map this else targs
          val appTp2 = derivedAppliedType(appTp, tycons, targs2)
          if (needsTopLevelNull(tycons)) appTp2.toJavaNullableUnion else appTp2
        case tp: LambdaType => mapOver(tp)
        case tp: TypeAlias => mapOver(tp)
        case tp @ AndType(tp1, tp2) =>
          // nullify(A & B) = (nullify(A) & nullify(B)) | JavaNull, but take care not to add
          // duplicate `JavaNull`s at the outermost level inside `A` and `B`.
          alreadyNullable = true
          derivedAndType(tp, this(tp1), this(tp2)).toJavaNullableUnion
        case tp @ OrType(tp1, tp2) if !tp.isJavaNullableUnion =>
          alreadyNullable = true
          derivedOrType(tp, this(tp1), this(tp2)).toJavaNullableUnion
        case tp: TypeParamRef if needsTopLevelNull(tp) => tp.toJavaNullableUnion
        case _ => tp
      }
    }
  }
}
