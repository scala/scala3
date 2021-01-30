import scala.collection.immutable.*

import scala.collection.mutable.{ Builder, ListBuffer }

object Test {

  private val defaultOrdering = Map[Numeric[_], Ordering[_]](
    Numeric.BigIntIsIntegral -> Ordering.BigInt,
    Numeric.IntIsIntegral -> Ordering.Int
  )

  final implicit class ArrowAssoc[A](private val self: A) extends AnyVal {
    inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
    def →[B](y: B): Tuple2[A, B] = ->(y)
  }

  def main(args: Array[String]): Unit = {
    assert((1 -> 2) == (1, 2))
    assert((1 → 2) == (1, 2))
  }


}

