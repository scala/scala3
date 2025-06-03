import scala.deriving._

trait TupleConversion[A, B] {
  def to(a: A): B
  def from(b: B): A
}

object TupleConversion  {
  inline given autoTupleConversion: [Prod <: Product] => (m: Mirror.ProductOf[Prod]) => TupleConversion[Prod, m.MirroredElemTypes] =
    new TupleConversion[Prod, m.MirroredElemTypes] {
      def to(a: Prod): m.MirroredElemTypes = Tuple.fromProductTyped(a)
      def from(b: m.MirroredElemTypes): Prod = m.fromProduct(b)
    }
}

final case class Data(s0: Int, s1: Int)

abstract class BaseSpec(f: () => Unit)

object ProductBuilderTest
  extends BaseSpec(() => {
    val conv = implicitly[TupleConversion[Data, (Int, Int)]]
  })