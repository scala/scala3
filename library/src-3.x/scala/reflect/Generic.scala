package scala.reflect

/** A class for mapping between an ADT value and
 *  the case mirror that represents the value.
 */
abstract class Generic[T] {

  type Shape <: scala.compiletime.Shape

  /** The case mirror corresponding to ADT instance `x` */
  def reflect(x: T): Mirror

  /** The ADT instance corresponding to given `mirror` */
  def reify(mirror: Mirror): T

  /** The companion object of the ADT */
  def common: GenericClass
}
