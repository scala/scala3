sealed trait Dim
trait _2D extends Dim
trait _3D extends Dim

sealed abstract class IntVector[D]
object IntVector{
  def apply[D](d: Array[Int]): IntVector[D] = ???
  def apply(x: Int, y: Int): IntVector2D = IntVector2D(x, y)
  def apply(x: Int, y: Int, z: Int): IntVector3D = IntVector3D(x, y, z)
}
case class IntVector2D(i: Int, j: Int) extends IntVector[_2D]
case class IntVector3D(i: Int, j: Int, k: Int) extends IntVector[_3D]

type DiscreteImage[D, A] = DiscreteField[D, DiscreteImageDomain, A]
class DiscreteField[D, DDomain[D] <: DiscreteDomain[D], A](val domain: DDomain[D], val data: IndexedSeq[A])  extends PartialFunction[PointId, A] {
  override def apply(v1: PointId) = ???
  override def isDefinedAt(ptId: PointId) = ???
}
object DiscreteField{
    implicit class DiscreteImageOps[D, A](discreteField: DiscreteField[D, DiscreteImageDomain, A]) {
      def apply(idx: IntVector[D]): A = ???
      def isDefinedAt(idx: IntVector[D]): Boolean = ???
  }
}
trait DiscreteDomain[D]
trait DiscreteImageDomain[D] extends DiscreteDomain[D]
final case class PointId(id: Int) extends AnyVal

def test[S](img: DiscreteImage[_3D, S]) = img(IntVector(1, 2, 3))
