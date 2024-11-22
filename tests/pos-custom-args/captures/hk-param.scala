//> using options -source 3.5
// (to make sure we use the unsealed policy)
/** Concrete collection type: View */
trait View[+A] extends Itable[A], ILike[A, [X] =>> View[X]^]:
  override def fromIterable[B](c: Itable[B]^): View[B]^{c} = ???

trait IPolyTransforms[+A, +C[A]] extends Any:
  def fromIterable[B](coll: Itable[B]^): C[B]

trait ILike[+A, +C[X] <: Itable[X]^] extends IPolyTransforms[A, C]

/** Base trait for generic collections */
trait Itable[+A] extends ItableOnce[A] with ILike[A, Itable^]

/** Iterator can be used only once */
trait ItableOnce[+A] {
  def iterator: Iterator[A]^{this}
}
