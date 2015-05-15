package dotty.tools.dotc
package core

import Names._, Types._, Contexts._, StdNames._
import TypeErasure.sigName

/** The signature of a denotation.
 *  Overloaded denotations with the same name are distinguished by
 *  their signatures. A signature of a method (of type PolyType,MethodType, or ExprType) is
 *  composed of a list of signature names, one for each parameter type, plus a signature for
 *  the result type. Methods are uncurried before taking their signatures.
 *  The signature name of a type is the fully qualified name of the type symbol of the type's erasure.
 *
 *  For instance a definition
 *
 *      def f(x: Int)(y: List[String]): String
 *
 *  would have signature
 *
 *      Signature(
 *        List("scala.Int".toTypeName, "scala.collection.immutable.List".toTypeName),
 *        "scala.String".toTypeName)
 *
 *  The signatures of non-method types are always `NotAMethod`.
 */
case class Signature(paramsSig: List[TypeName], resSig: TypeName) {

  /** Does this signature coincide with that signature on their parameter parts? */
  final def sameParams(that: Signature): Boolean = this.paramsSig == that.paramsSig

  /** The meaning of `matches` depends on the phase. If types are not erased,
   *  it means `sameParams`. Once types are erased, it means `==`, comparing parameter as
   *  well as result type parts.
   */
  final def matches(that: Signature)(implicit ctx: Context) =
    if (ctx.erasedTypes) equals(that) else sameParams(that)

  /** Construct a signature by prepending the signature names of the given `params`
   *  to the parameter part of this signature.
   */
  def prepend(params: List[Type], isJava: Boolean)(implicit ctx: Context) =
    Signature((params.map(sigName(_, isJava))) ++ paramsSig, resSig)

}

object Signature {

  /** The signature of everything that's not a method, i.e. that has
   *  a type different from PolyType, MethodType, or ExprType.
   */
  val NotAMethod = Signature(List(), EmptyTypeName)

  /** The signature of an overloaded denotation.
   */
  val OverloadedSignature = Signature(List(tpnme.OVERLOADED), EmptyTypeName)

  /** The signature of a method with no parameters and result type `resultType`. */
  def apply(resultType: Type, isJava: Boolean)(implicit ctx: Context): Signature = {
    assert(!resultType.isInstanceOf[ExprType])
    apply(Nil, sigName(resultType, isJava))
  }
}
