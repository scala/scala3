package dotty.tools.dotc
package core

import Types._, Contexts._, Flags._, Symbols._, Annotations._
import TypeApplications.TypeParamInfo
import Decorators._
import cc.CapturingType

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

  /** Map everything below Bivariant to Invariant */
  def cut(v: Variance): Variance =
    if (v == Bivariant) v else Invariant

  def compose(v: Variance, boundsVariance: Int): Variance =
    if (boundsVariance == 1) v
    else if (boundsVariance == -1) flip(v)
    else cut(v)

  /** Compute variance of type parameter `tparam` in types of all symbols `sym`. */
  def varianceInSyms(syms: List[Symbol])(tparam: Symbol)(using Context): Variance =
    syms.foldLeft(Bivariant) ((v, sym) => v & varianceInSym(sym)(tparam))

  /** Compute variance of type parameter `tparam` in type of symbol `sym`. */
  def varianceInSym(sym: Symbol)(tparam: Symbol)(using Context): Variance =
    if (sym.isAliasType) cut(varianceInType(sym.info)(tparam))
    else varianceInType(sym.info)(tparam)

  /** Compute variance of type parameter `tparam` in all types `tps`. */
  def varianceInTypes(tps: List[Type])(tparam: Symbol)(using Context): Variance =
    tps.foldLeft(Bivariant) ((v, tp) => v & varianceInType(tp)(tparam))

  /** Compute variance of type parameter `tparam` in all type arguments
   *  <code>tps</code> which correspond to formal type parameters `tparams1`.
   */
  def varianceInArgs(tps: List[Type], tparams1: List[Symbol])(tparam: Symbol)(using Context): Variance = {
    var v: Variance = Bivariant;
    for ((tp, tparam1) <- tps zip tparams1) {
      val v1 = varianceInType(tp)(tparam)
      v = v & (if (tparam1.is(Covariant)) v1
           else if (tparam1.is(Contravariant)) flip(v1)
           else cut(v1))
    }
    v
  }

  /** Compute variance of type parameter `tparam` in all type annotations `annots`. */
  def varianceInAnnots(annots: List[Annotation])(tparam: Symbol)(using Context): Variance =
    annots.foldLeft(Bivariant) ((v, annot) => v & varianceInAnnot(annot)(tparam))

  /** Compute variance of type parameter `tparam` in type annotation `annot`. */
  def varianceInAnnot(annot: Annotation)(tparam: Symbol)(using Context): Variance =
    varianceInType(annot.tree.tpe)(tparam)

  /** Compute variance of type parameter <code>tparam</code> in type <code>tp</code>. */
  def varianceInType(tp: Type)(tparam: Symbol)(using Context): Variance = tp match {
    case TermRef(pre, _) =>
      varianceInType(pre)(tparam)
    case tp @ TypeRef(pre, _) =>
      if (tp.symbol == tparam) Covariant else varianceInType(pre)(tparam)
    case tp @ TypeBounds(lo, hi) =>
      if (lo eq hi) cut(varianceInType(hi)(tparam))
      else flip(varianceInType(lo)(tparam)) & varianceInType(hi)(tparam)
    case tp @ RefinedType(parent, _, rinfo) =>
      varianceInType(parent)(tparam) & varianceInType(rinfo)(tparam)
    case tp: RecType =>
      varianceInType(tp.parent)(tparam)
    case tp: MethodOrPoly =>
      flip(varianceInTypes(tp.paramInfos)(tparam)) & varianceInType(tp.resultType)(tparam)
    case ExprType(restpe) =>
      varianceInType(restpe)(tparam)
    case tp @ AppliedType(tycon, args) =>
      def varianceInArgs(v: Variance, args: List[Type], tparams: List[ParamInfo]): Variance =
        args match {
          case arg :: args1 =>
            varianceInArgs(
              v & compose(varianceInType(arg)(tparam), tparams.head.paramVarianceSign),
              args1, tparams.tail)
          case nil =>
            v
        }
      varianceInArgs(varianceInType(tycon)(tparam), args, tycon.typeParams)
    case CapturingType(tp, _) =>
      varianceInType(tp)(tparam)
    case AnnotatedType(tp, annot) =>
      varianceInType(tp)(tparam) & varianceInAnnot(annot)(tparam)
    case AndType(tp1, tp2) =>
      varianceInType(tp1)(tparam) & varianceInType(tp2)(tparam)
    case OrType(tp1, tp2) =>
      varianceInType(tp1)(tparam) & varianceInType(tp2)(tparam)
    case _ =>
      Bivariant
  }

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

  def varianceSign(sym: Symbol)(using Context): String =
    varianceSign(sym.variance)

  def varianceSign(v: Variance): String = varianceSign(varianceToInt(v))

  def varianceSign(v: Int): String =
    if (v > 0) "+"
    else if (v < 0) "-"
    else ""

  val alwaysInvariant: Any => Invariant.type = Function.const(Invariant)
}
