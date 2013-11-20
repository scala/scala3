package dotty.tools
package dotc
package core

import Types._, Contexts._
import util.SimpleMap
import collection.mutable.ListBuffer
import printing.{Printer, Showable}
import printing.Texts._

/** Constraint over undetermined type parameters
 *  @param myMap a map from PolyType to the type bounds that constrain the
 *               polytype's type parameters. A type parameter that does not
 *               have a constraint is represented by a `NoType` in the corresponding
 *               array entry.
 */
class Constraint(val myMap: SimpleMap[PolyType, Array[Type]]) extends AnyVal with Showable {

  /** Does the constraint's domain contain the type parameters of `pt`? */
  def contains(pt: PolyType): Boolean = myMap(pt) != null

  /** Does the constraint's domain contain the type parameter `param`? */
  def contains(param: PolyParam): Boolean = {
    val entries = myMap(param.binder)
    entries != null && entries(param.paramNum).exists
  }

  /** The constraint for given type parameter `param`, or NoType if `param` is not part of
   *  the constraint domain.
   */
  def at(param: PolyParam): Type = {
    val entries = myMap(param.binder)
    if (entries == null) NoType else entries(param.paramNum)
  }

  /** The constraint bounds for given type parameter `param`.
   *  @pre `param` is not part of the constraint domain.
   */
  def bounds(param: PolyParam): TypeBounds = at(param).asInstanceOf[TypeBounds]

  /** A new constraint which is derived from this constraint by adding or replacing
   *  the entries corresponding to `pt` with `entries`.
   */
  private def updateEntries(pt: PolyType, entries: Array[Type]): Constraint = {
    import Constraint._
    val res = new Constraint(myMap.updated(pt, entries))
    if (res.myMap.size > maxSize) {
      maxSize = res.myMap.size
      maxConstraint = res
    }
    res
  }

  /** A new constraint which is derived from this constraint by updating
   *  the the entry for parameter `param` to `tpe`.
   *  @pre  `this contains param`.
   */
  def updated(param: PolyParam, tpe: Type): Constraint = {
    val newEntries = myMap(param.binder).clone
    newEntries(param.paramNum) = tpe
    updateEntries(param.binder, newEntries)
  }

  /** A new constraint which is derived from this constraint by mapping
   *  `op` over all entries of type `poly`.
   *  @pre  `this contains poly`.
   */
  def transformed(poly: PolyType, op: Type => Type): Constraint =
    updateEntries(poly, myMap(poly) map op)

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain.
   */
  def - (param: PolyParam)(implicit ctx: Context) = {
    val pt = param.binder
    val pnum = param.paramNum
    val entries = myMap(pt)
    var noneLeft = true
    var i = 0
    while (noneLeft && (i < entries.length)) {
      noneLeft = (entries(i) eq NoType) || i == pnum
      i += 1
    }
    if (noneLeft) new Constraint(myMap remove pt)
    else updated(param, NoType)
  }

  /** A new constraint which is derived from this constraint by adding
   *  entries for all type parameters of `poly`.
   */
  def +(poly: PolyType) =
    updateEntries(poly, poly.paramBounds.toArray[Type])

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

    new Constraint((this - param).myMap mapValues subst)
  }

  def domainPolys: List[PolyType] = myMap.keys

  def domainParams: List[PolyParam] =
    for {
      (poly, entries) <- myMap.toList
      n <- 0 until entries.length
      if entries(n).exists
    } yield PolyParam(poly, n)

  def constrainedTypesText(printer: Printer): Text =
    Text(domainPolys map (_.toText(printer)), ", ")

  def constraintText(indent: Int, printer: Printer): Text = {
    val assocs =
      for (param <- domainParams)
      yield (" " * indent) ~ param.toText(printer) ~ at(param).toText(printer)
    Text(assocs, "\n")
  }

  override def toText(printer: Printer): Text =
    "Constraint(" ~ constrainedTypesText(printer) ~ ") {" ~ constraintText(2, printer) ~ "}"
}

object Constraint {
  var maxSize = 0
  var maxConstraint: Constraint = _
  def printMax()(implicit ctx: Context) =
    if (maxSize > 0) println(s"max constraint = ${maxConstraint.show}")
}
