/** Concrete collection type: View */
trait View[+A] extends Itable[A], ILike[A, [X] =>> View[X]^]: // error
  override def fromIterable[B](c: Itable[B]^): View[B]^{c} = ??? // error

trait IPolyTransforms[+A, +C[A]] extends Any:
  def fromIterable[B](coll: Itable[B]^): C[B]

trait ILike[+A, +C[X] <: Itable[X]^] extends IPolyTransforms[A, C]

/** Base trait for generic collections */
trait Itable[+A] extends ItableOnce[A] with ILike[A, Itable^] // error

/** Iterator can be used only once */
trait ItableOnce[+A] {
  def iterator: Iterator[A]^{this}
}
