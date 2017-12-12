package dotty.generic

/** A generic representation of `A` as a sum of products. Provides back and
 *  forth conversions between values of type `A` and values of type `Repr`.
 *  Instances of `Representable` for sealed traits and case classes are
 *  automatically synthesized by the compiler when requested implicitly.
 */
trait Representable[A] {
  /** Type of the generic representation of `A`, composed of `Sum` and `Prod` types. */
  type Repr // <: Sum | Prod ← when we stop cross compiling

  /** Converts a value of type `A` to it's generic representation */
  def to(a: A): Repr

  /** Converts a value in the generic representation of `A` to its concrete representation. */
  def from(r: Repr): A
}

object Representable {
  def apply[A](implicit r: Representable[A]): r.type = r
}
