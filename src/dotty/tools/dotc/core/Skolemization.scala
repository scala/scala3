package dotty.tools.dotc
package core

import Symbols._, Types._, Contexts._
import collection.mutable

/** Methods to add and remove skolemtypes.
 *
 *  Skolem types are generated when comparing refinements.
 *  A skolem type is simply a fresh singleton type that has a given type
 *  as underlying type.
 *  Two skolem types are equal if they refer to the same underlying type.
 *  To avoid unsoundness, skolem types have to be kept strictly local to the
 *  comparison, they are not allowed to escape the lifetime of a comparison
 *  by surviving in a context or in GADT bounds.
 */
trait Skolemization {

  implicit val ctx: Context

  protected var skolemsOutstanding = false

  def ensureStableSingleton(tp: Type): SingletonType = tp.stripTypeVar match {
    case tp: SingletonType if tp.isStable =>
      tp
    case tp: ValueType =>
      skolemsOutstanding = true
      SkolemType(tp)
    case tp: TypeProxy =>
      ensureStableSingleton(tp.underlying)
  }

  /** Approximate a type `tp` with a type that does not contain skolem types.
   *  @param  toSuper   if true, return the smallest supertype of `tp` with this property
   *                    else return the largest subtype.
   */
  final def deSkolemize(tp: Type, toSuper: Boolean): Type =
    if (skolemsOutstanding) deSkolemize(tp, if (toSuper) 1 else -1, Set())
    else tp

  private def deSkolemize(tp: Type, variance: Int, seen: Set[SkolemType]): Type =
    ctx.traceIndented(s"deskolemize $tp, variance = $variance, seen = $seen  =  ") {
    def approx(lo: Type = defn.NothingType, hi: Type = defn.AnyType, newSeen: Set[SkolemType] = seen) =
      if (variance == 0) NoType
      else deSkolemize(if (variance < 0) lo else hi, variance, newSeen)
    tp match {
      case tp: SkolemType =>
        if (seen contains tp) NoType
        else approx(hi = tp.binder, newSeen = seen + tp)
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym.isStatic) tp
        else {
          val pre1 = deSkolemize(tp.prefix, variance, seen)
          if (pre1.exists && !pre1.isRef(defn.NothingClass)) tp.derivedSelect(pre1)
          else {
            ctx.log(s"deskolem: $tp: ${tp.info}")
            tp.info match {
              case TypeBounds(lo, hi) => approx(lo, hi)
              case info => approx(defn.NothingType, info)
            }
          }
        }
      case _: ThisType | _: BoundType | _: SuperType | NoType | NoPrefix =>
        tp
      case tp: RefinedType =>
        val parent1 = deSkolemize(tp.parent, variance, seen)
        if (parent1.exists) {
          val refinedInfo1 = deSkolemize(tp.refinedInfo, variance, seen)
          if (refinedInfo1.exists)
            tp.derivedRefinedType(parent1, tp.refinedName, refinedInfo1)
          else
            approx(hi = parent1)
        }
        else approx()
      case tp: TypeAlias =>
        val alias1 = deSkolemize(tp.alias, variance * tp.variance, seen)
        if (alias1.exists) tp.derivedTypeAlias(alias1)
        else approx(hi = TypeBounds.empty)
      case tp: TypeBounds =>
        val lo1 = deSkolemize(tp.lo, -variance, seen)
        val hi1 = deSkolemize(tp.hi, variance, seen)
        if (lo1.exists && hi1.exists) tp.derivedTypeBounds(lo1, hi1)
        else approx(hi =
          if (lo1.exists) TypeBounds.lower(lo1)
          else if (hi1.exists) TypeBounds.upper(hi1)
          else TypeBounds.empty)
      case tp: ClassInfo =>
        val pre1 = deSkolemize(tp.prefix, variance, seen)
        if (pre1.exists) tp.derivedClassInfo(pre1)
        else NoType
      case tp: AndOrType =>
        val tp1d = deSkolemize(tp.tp1, variance, seen)
        val tp2d = deSkolemize(tp.tp2, variance, seen)
        if (tp1d.exists && tp2d.exists)
          tp.derivedAndOrType(tp1d, tp2d)
        else if (tp.isAnd)
          approx(hi = tp1d & tp2d)  // if one of tp1d, tp2d exists, it is the result of tp1d & tp2d
        else
          approx(lo = tp1d & tp2d)
      case tp: WildcardType =>
        val bounds1 = deSkolemize(tp.optBounds, variance, seen)
        if (bounds1.exists) tp.derivedWildcardType(bounds1)
        else WildcardType
      case _ =>
        if (tp.isInstanceOf[MethodicType]) assert(variance != 0, tp)
        deSkolemizeMap.mapOver(tp, variance, seen)
    }
  }

  object deSkolemizeMap extends TypeMap {
    private var seen: Set[SkolemType] = _
    def apply(tp: Type) = deSkolemize(tp, variance, seen)
    def mapOver(tp: Type, variance: Int, seen: Set[SkolemType]) = {
      val savedVariance = this.variance
      val savedSeen = this.seen
      this.variance = variance
      this.seen = seen
      try super.mapOver(tp)
      finally {
        this.variance = savedVariance
        this.seen = savedSeen
      }
    }
  }
}
