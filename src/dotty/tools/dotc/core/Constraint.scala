package dotty.tools
package dotc
package core

import Types._, Contexts._
import util.SimpleMap
import collection.mutable.ListBuffer
import printing.{Printer, Showable}
import printing.Texts._

/** Constraint over undetermined type parameters
 *  @param map  a map from PolyType to the type bounds that constrain the
 *              polytype's type parameters. A type parameter that does not
 *              have a constraint is represented by a `NoType` in the corresponding
 *              array entry.
 */
class Constraint(val map: SimpleMap[PolyType, Array[Type]]) extends AnyVal with Showable {

  /** Does the constraint's domain contain the type parameters of `pt`? */
  def contains(pt: PolyType): Boolean = map(pt) != null

  /** The constraint for given type parameter `param`, or NoType if `param` is not part of
   *  the constraint domain.
   */
  def apply(param: PolyParam): Type = {
    val entries = map(param.binder)
    if (entries == null) NoType else entries(param.paramNum)
  }

  /** The constraint for the type parameters of `pt`.
   *  @pre  The polytype's type parameters are contained in the constraint's domain.
   */
  def apply(pt: PolyType): Array[Type] = map(pt)

  /** A new constraint which is derived from this constraint by adding or replacing
   *  the entries corresponding to `pt` with `entries`.
   */
  def updated(pt: PolyType, entries: Array[Type]) = {
    assert(map(pt) != null)
    new Constraint(map.updated(pt, entries))
  }

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain.
   */
  def - (param: PolyParam) = {
    val pt = param.binder
    val pnum = param.paramNum
    val entries = map(pt)
    var noneLeft = true
    var i = 0
    while (noneLeft && (i < entries.length)) {
      noneLeft = (entries(i) eq NoType) || i == pnum
      i += 1
    }
    new Constraint(
      if (noneLeft) map remove pt
      else {
        val newEntries = entries.clone
        newEntries(pnum) = NoType
        map.updated(pt, newEntries)
      })
  }

  def +(pt: PolyType) = {
/*
    pt.resultType match {
      case MethodType(pname :: rest, _) =>
        assert(pname.toString != "__thingToAdd")
      case _ =>
    }
*/
    new Constraint(map.updated(pt, pt.paramBounds.toArray))
  }
  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all occurrences
   *  of the parameter elsewhere in the constraint by type `tp`.
   */
  def replace(param: PolyParam, tp: Type)(implicit ctx: Context) = {
    def subst(entries: Array[Type]) = {
      var result = entries
      var i = 0
      while (i < entries.length) {
        entries(i) match {
          case oldBounds: TypeBounds =>
            val newBounds = oldBounds.substParam(param, tp)
            if (oldBounds ne newBounds) {
              if (result eq entries) result = entries.clone
              result(i) = newBounds
            }
          case _ =>
        }
        i += 1
      }
      result
    }

    new Constraint((this - param).map mapValues subst)
  }

  def domainPolys: List[PolyType] = map.keys

  def domainParams: List[PolyParam] =
    for {
      (poly, entries) <- map.toList
      n <- 0 until entries.length
      if entries(n).exists
    } yield PolyParam(poly, n)

  override def toText(printer: Printer): Text = {
    val dom = domainPolys map (_.toText(printer))
    val assocs =
      for (param <- domainParams)
      yield "  " ~ param.toText(printer) ~ this(param).toText(printer)
    "Constraint(" ~ Text(dom, ", ") ~ ") {" ~ Text(assocs, "\n") ~ "}"
  }
}
