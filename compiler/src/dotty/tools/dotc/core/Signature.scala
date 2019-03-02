package dotty.tools.dotc
package core

import Names._, Types._, Contexts._, StdNames._, Decorators._
import TypeErasure.sigName

import scala.annotation.tailrec

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
 *
 *  There are three kinds of "missing" parts of signatures:
 *
 *   - tpnme.EMPTY          Result type marker for NotAMethod and OverloadedSignature
 *   - tpnme.WILDCARD       Arises from a Wildcard or error type
 *   - tpnme.Uninstantiated Arises from an uninstantiated type variable
 */
case class Signature(paramsSig: List[TypeName], resSig: TypeName) {
  import Signature._

  /** Two names are consistent if they are the same or one of them is tpnme.Uninstantiated */
  private def consistent(name1: TypeName, name2: TypeName) =
    name1 == name2 || name1 == tpnme.Uninstantiated || name2 == tpnme.Uninstantiated

  /** Does this signature coincide with that signature on their parameter parts?
   *  This is the case if all parameter names are _consistent_, i.e. they are either
   *  equal or on of them is tpnme.Uninstantiated.
   */
  final def consistentParams(that: Signature)(implicit ctx: Context): Boolean = {
    @tailrec def loop(names1: List[TypeName], names2: List[TypeName]): Boolean =
      if (names1.isEmpty) names2.isEmpty
      else !names2.isEmpty && consistent(names1.head, names2.head) && loop(names1.tail, names2.tail)
    if (ctx.erasedTypes && (this == NotAMethod) != (that == NotAMethod))
      false // After erasure, we allow fields and parameterless methods with the same name.
            // This is needed to allow both a module field and a bridge method for an abstract val.
            // Test case is patmatch-classtag.scala
    else
      loop(this.paramsSig, that.paramsSig)
  }

  /** `that` signature, but keeping all corresponding parts of `this` signature. */
  final def updateWith(that: Signature): Signature = {
    def update(name1: TypeName, name2: TypeName): TypeName =
      if (consistent(name1, name2)) name1 else name2
    if (this == that) this
    else if (!this.paramsSig.hasSameLengthAs(that.paramsSig)) that
    else {
      val mapped = Signature(
          this.paramsSig.zipWithConserve(that.paramsSig)(update),
          update(this.resSig, that.resSig))
      if (mapped == this) this else mapped
    }
  }

  /** The degree to which this signature matches `that`.
   *  If parameter names are consistent and result types names match (i.e. they are the same
   *  or one is a wildcard), the result is `FullMatch`.
   *  If only the parameter names are consistent, the result is `ParamMatch` before erasure and
   *  `NoMatch` otherwise.
   *  If the parameters are inconsistent, the result is always `NoMatch`.
   */
  final def matchDegree(that: Signature)(implicit ctx: Context): MatchDegree =
    if (consistentParams(that))
      if (resSig == that.resSig || isWildcard(resSig) || isWildcard(that.resSig)) FullMatch
      else if (!ctx.erasedTypes) ParamMatch
      else NoMatch
    else NoMatch

  /** Does this signature potentially clash with `that` ? */
  def clashes(that: Signature)(implicit ctx: Context): Boolean =
    matchDegree(that) == FullMatch

  /** name.toString == "" or name.toString == "_" */
  private def isWildcard(name: TypeName) = name.isEmpty || name == tpnme.WILDCARD

  /** Construct a signature by prepending the signature names of the given `params`
   *  to the parameter part of this signature.
   *
   *  Like Signature#apply, the result is only cacheable if `isUnderDefined == false`.
   */
  def prepend(params: List[Type], isJava: Boolean)(implicit ctx: Context): Signature =
    Signature(params.map(p => sigName(p, isJava)) ++ paramsSig, resSig)

  /** A signature is under-defined if its paramsSig part contains at least one
   *  `tpnme.Uninstantiated`. Under-defined signatures arise when taking a signature
   *  of a type that still contains uninstantiated type variables.
   */
  def isUnderDefined(implicit ctx: Context): Boolean =
    paramsSig.contains(tpnme.Uninstantiated) || resSig == tpnme.Uninstantiated
}

object Signature {

  type MatchDegree = Int
  val NoMatch: Int = 0
  val ParamMatch: Int = 1
  val FullMatch: Int = 2

  /** The signature of everything that's not a method, i.e. that has
   *  a type different from PolyType, MethodType, or ExprType.
   */
  val NotAMethod: Signature = Signature(List(), EmptyTypeName)

  /** The signature of an overloaded denotation.
   */
  val OverloadedSignature: Signature = Signature(List(tpnme.OVERLOADED), EmptyTypeName)

  /** The signature of a method with no parameters and result type `resultType`.
   *
   *  The resulting value is only cacheable if `isUnderDefined == false`,
   *  otherwise the signature will change once the contained type variables have
   *  been instantiated.
   */
  def apply(resultType: Type, isJava: Boolean)(implicit ctx: Context): Signature = {
    assert(!resultType.isInstanceOf[ExprType])
    apply(Nil, sigName(resultType, isJava))
  }
}
