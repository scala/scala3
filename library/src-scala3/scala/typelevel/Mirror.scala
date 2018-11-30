package scala.typelevel

/** A generic representation of a case in an ADT
  *  @param  deriving  The companion object of the ADT
  *  @param  ordinal   The ordinal value of the case in the list of the ADT's cases
  *  @param  elems     The elements of the case
  */
class Mirror(val reflected: ReflectedClass, val ordinal: Int, val elems: Product) {

  /** The `n`'th element of this generic case */
  def apply(n: Int): Any = elems.productElement(n)

  /** The name of the constructor of the case reflected by this mirror */
  def caseLabel: String = reflected.label(ordinal, 0)

  /** The label of the `n`'th element of the case reflected by this mirror */
  def elementLabel(n: Int) = reflected.label(ordinal, n + 1)
}
