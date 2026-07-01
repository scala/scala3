package dotty.tools.dotc
package core

import scala.annotation.tailrec

import Names.*, Types.*, Contexts.*, StdNames.*, Decorators.*
import TypeErasure.sigName
import Signature.*

/** The signature of a denotation.
 *
 *  Same-named denotations with different signatures are considered to be
 *  overloads, see `SingleDenotation#matches` for more details.
 *
 *  A _method signature_ (a value of type `Signature`, excluding `NotAMethod`
 *  and `OverloadedSignature`) is composed of a list of _parameter signatures_,
 *  plus a _type signature_ for the final result type.
 *
 *  A _parameter signature_ (a value of type `ParamSig`) is either an integer,
 *  representing the number of type parameters in a type parameter section, or
 *  the _type signature_ of a term parameter.
 *
 *  A _type signature_ is the fully qualified name of the type symbol of the
 *  type's erasure.
 *
 *  For instance a definition
 *
 *      def f[T, S](x: Int)(y: List[T]): S
 *
 *  would have signature
 *
 *      Signature(
 *        List(2, "scala.Int".toTypeName, "scala.collection.immutable.List".toTypeName),
 *        "java.lang.Object".toTypeName)
 *
 *  Note that `paramsSig` has one entry for *a whole type parameter section* but
 *  one entry *for each term parameter* (currently, methods in Dotty can only
 *  have one type parameter section but this encoding leaves the door open for
 *  supporting multiple sections).
 *
 *  The signatures of non-method types are always `NotAMethod`.
 *
 *  There are three kinds of "missing" parts of signatures:
 *
 *   - tpnme.EMPTY          Result type marker for NotAMethod and OverloadedSignature
 *   - tpnme.WILDCARD       Arises from a Wildcard or error type
 *   - tpnme.Uninstantiated Arises from an uninstantiated type variable
 */
case class Signature(paramsSig: List[ParamSig], resSig: TypeName) {

  /** Two names are consistent if they are the same or one of them is tpnme.Uninstantiated.
   *
   *  `ParamSig = TypeName | Int` erases to `AnyRef`, so a plain `==` compiles to
   *  `BoxesRunTime.equals(_, _)` and dispatches through `equals2` (instanceof Number /
   *  Character / null / virtual `Object.equals`). The dominant `TypeName` arm
   *  collapses to `eq` once that chain unwinds (Names.scala:157), so we
   *  hand-write a typed-arm match that emits `if_acmpeq` / `if_icmpeq` directly
   *  and never re-enters the boxing helper.
   */
  private def consistent(name1: ParamSig, name2: ParamSig): Boolean =
    name1 match
      case n1: TypeName =>
        name2 match
          case n2: TypeName => (n1 eq n2) || (n1 eq tpnme.Uninstantiated) || (n2 eq tpnme.Uninstantiated)
          case _: Int      => (n1 eq tpnme.Uninstantiated)
      case i1: Int =>
        name2 match
          case _: TypeName => name2.asInstanceOf[AnyRef] eq tpnme.Uninstantiated
          case i2: Int     => i1 == i2

  /** Does this signature coincide with that signature on their parameter parts?
   *  This is the case if all parameter signatures are _consistent_, i.e. they are either
   *  equal or on of them is tpnme.Uninstantiated.
   */
  final def consistentParams(that: Signature)(using Context): Boolean = {
    @tailrec def loop(names1: List[ParamSig], names2: List[ParamSig]): Boolean =
      if (names1.isEmpty) names2.isEmpty
      else !names2.isEmpty && consistent(names1.head, names2.head) && loop(names1.tail, names2.tail)
    loop(this.paramsSig, that.paramsSig)
  }

  /** `that` signature, but keeping all corresponding parts of `this` signature. */
  final def updateWith(that: Signature): Signature = {
    def update[T <: ParamSig](name1: T, name2: T): T =
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
   *  If parameter signatures are consistent and result types names match (i.e. they are the same
   *  or one is a wildcard), the result is `FullMatch`.
   *  If only the parameter signatures are consistent, the result is either
   *  `MethodNotAMethodMatch` (if one side is a method signature and the other isn't),
   *  or `ParamMatch`.
   *  If the parameters are inconsistent, the result is always `NoMatch`.
   */
  final def matchDegree(that: Signature)(using Context): MatchDegree =
    if consistentParams(that) then
      if (resSig eq that.resSig) || isWildcard(resSig) || isWildcard(that.resSig) then
        FullMatch
      else if (this == NotAMethod) != (that == NotAMethod) then
        MethodNotAMethodMatch
      else
        ParamMatch
    else
      NoMatch

  /** Does this signature potentially clash with `that` ? */
  def clashes(that: Signature)(using Context): Boolean =
    matchDegree(that) == FullMatch

  private def isWildcard(name: TypeName) = name eq tpnme.WILDCARD

  /** Construct a signature by prepending the signature names of the given `params`
   *  to the parameter part of this signature.
   *
   *  Like Signature#apply, the result is only cacheable if `isUnderDefined == false`.
   */
  def prependTermParams(params: List[Type], sourceLanguage: SourceLanguage)(using Context): Signature =
    Signature(params.map(p => sigName(p, sourceLanguage)) ::: paramsSig, resSig)

  /** Construct a signature by prepending the length of a type parameter section
   *  to the parameter part of this signature.
   *
   *  Like Signature#apply, the result is only cacheable if `isUnderDefined == false`.
   */
  def prependTypeParams(typeParamSigsSectionLength: Int)(using Context): Signature =
    Signature(typeParamSigsSectionLength :: paramsSig, resSig)

  /** A signature is under-defined if its paramsSig part contains at least one
   *  `tpnme.Uninstantiated`. Under-defined signatures arise when taking a signature
   *  of a type that still contains uninstantiated type variables.
   */
  def isUnderDefined(using Context): Boolean =
    containsUninstantiated(paramsSig) || (resSig eq tpnme.Uninstantiated)

  /** Same as `paramsSig.contains(tpnme.Uninstantiated)` but typed on the union arms
   *  so the per-element comparator emits `if_acmpeq` instead of going through
   *  `BoxesRunTime.equals` for each element. Int arms can never equal the
   *  TypeName `tpnme.Uninstantiated` and are skipped without dispatch.
   */
  @tailrec private def containsUninstantiated(xs: List[ParamSig]): Boolean = xs match
    case Nil          => false
    case (n: TypeName) :: rest => (n eq tpnme.Uninstantiated) || containsUninstantiated(rest)
    case _ :: rest    => containsUninstantiated(rest)
}

object Signature {
  /** A parameter signature, see the documentation of `Signature` for more information. */
  type ParamSig = TypeName | Int
    // Erasure means that our Ints will be boxed, but Integer#valueOf caches
    // small values, so the performance hit should be minimal.

  enum MatchDegree {
    /** The signatures are unrelated. */
    case NoMatch
    /** The parameter signatures are equivalent. */
    case ParamMatch
    /** Both signatures have no parameters, one is a method and the other isn't.
     *
     *  @see NotAMethod
     */
    case MethodNotAMethodMatch
    /** The parameter and result type signatures are equivalent. */
    case FullMatch
  }
  export MatchDegree._

  /** The signature of everything that's not a method, i.e. that has
   *  a type different from PolyType or MethodType.
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
  def apply(resultType: Type, sourceLanguage: SourceLanguage)(using Context): Signature = {
    assert(!resultType.isInstanceOf[ExprType])
    apply(Nil, sigName(resultType, sourceLanguage))
  }

  val lexicographicOrdering: Ordering[Signature] = new Ordering[Signature] {
    val paramSigOrdering: Ordering[Signature.ParamSig] = new Ordering[Signature.ParamSig] {
      def compare(x: ParamSig, y: ParamSig): Int = x match { // `(x, y) match` leads to extra allocations
        case x: TypeName =>
          y match {
            case y: TypeName =>
              // `Ordering[TypeName]` doesn't work due to `Ordering` still being invariant
              summon[Ordering[Name]].compare(x, y)
            case y: Int =>
              1
          }
        case x: Int =>
          y match {
            case y: Name =>
              -1
            case y: Int =>
              x - y
          }
      }
    }
    def compare(x: Signature, y: Signature): Int = {
      import scala.math.Ordering.Implicits.seqOrdering
      val paramsOrdering = seqOrdering(using paramSigOrdering).compare(x.paramsSig, y.paramsSig)
      if (paramsOrdering != 0) paramsOrdering
      else summon[Ordering[Name]].compare(x.resSig, y.resSig)
    }
  }
}
