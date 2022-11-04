/** Concrete collection type: View */
trait View[+A] extends Itable[A], ILike[A, [X] =>> {*} View[X]]:
  override def fromIterable[B](c: {*} Itable[B]): {c} View[B] = ???

trait IPolyTransforms[+A, +C[A]] extends Any:
  def fromIterable[B](coll: {*} Itable[B]): C[B]

trait ILike[+A, +C[X] <: {*} Itable[X]] extends IPolyTransforms[A, C]

/** Base trait for generic collections */
trait Itable[+A] extends ItableOnce[A] with ILike[A, {*} Itable]

/** Iterator can be used only once */
trait ItableOnce[+A] {
  this: {*} ItableOnce[A] =>
  def iterator: {this} Iterator[A]
}
