package dotty.tools.dotc
package core

import Types._, Contexts._, Flags._, Symbols._, Annotations._
import TypeApplications.TypeParamInfo
import Decorators._

object Variances {

  type Variance = FlagSet
  val Bivariant: Variance = VarianceFlags
  val Invariant: Variance = EmptyFlags

  def varianceFromInt(v: Int): Variance =
    if v < 0 then Contravariant
    else if v > 0 then Covariant
    else Invariant

  def varianceToInt(v: Variance): Int =
    if v.is(Covariant) then 1
    else if v.is(Contravariant) then -1
    else 0

  /** Flip between covariant and contravariant */
  def flip(v: Variance): Variance =
    if (v == Covariant) Contravariant
    else if (v == Contravariant) Covariant
    else v

  def setStructuralVariances(lam: HKTypeLambda)(using Context): Unit =
    assert(!lam.isDeclaredVarianceLambda)
    for param <- lam.typeParams do param.storedVariance = Bivariant
    object narrowVariances extends TypeTraverser {
      def traverse(t: Type): Unit = t match
        case t: TypeParamRef if t.binder eq lam =>
          lam.typeParams(t.paramNum).storedVariance &= varianceFromInt(variance)
        case _ =>
          traverseChildren(t)
    }
    // Note: Normally, we'd need to repeat `traverse` until a fixpoint is reached.
    // But since recursive lambdas can only appear in bounds, and bounds never have
    // structural variances, a single traversal is enough.
    narrowVariances.traverse(lam.resType)

  /** Does variance `v1` conform to variance `v2`?
   *  This is the case if the variances are the same or `sym` is nonvariant.
   */
  def varianceConforms(v1: Int, v2: Int): Boolean =
    v1 == v2 || v2 == 0

  /** Does the variance of type parameter `tparam1` conform to the variance of type parameter `tparam2`?
   */
  def varianceConforms(tparam1: TypeParamInfo, tparam2: TypeParamInfo)(using Context): Boolean =
    tparam1.paramVariance.isAllOf(tparam2.paramVariance)

  /** Do the variances of type parameters `tparams1` conform to the variances
   *  of corresponding type parameters `tparams2`?
   *  This is only the case if `tparams1` and `tparams2` have the same length.
   */
  def variancesConform(tparams1: List[TypeParamInfo], tparams2: List[TypeParamInfo])(using Context): Boolean =
    val needsDetailedCheck = tparams2 match
      case (_: Symbol) :: _ => true
      case LambdaParam(tl: HKTypeLambda, _) :: _ => tl.isDeclaredVarianceLambda
      case _ => false
    if needsDetailedCheck then tparams1.corresponds(tparams2)(varianceConforms)
    else tparams1.hasSameLengthAs(tparams2)

  def varianceSign(v: Variance): String = varianceSign(varianceToInt(v))
  def varianceLabel(v: Variance): String = varianceLabel(varianceToInt(v))

  def varianceSign(v: Int): String =
    if (v > 0) "+"
    else if (v < 0) "-"
    else ""

  def varianceLabel(v: Int): String =
    if v < 0 then "contravariant"
    else if v > 0 then "covariant"
    else "invariant"

  val alwaysInvariant: Any => Invariant.type = Function.const(Invariant)
}
