package scala.collection

import org.junit.Assert.*
import org.junit.Test

class SortedSetTest {
  import SortedSetTest.*

  private var count: Int = 0
  private def genOrd: Ordering[Box[Int]] =
    Ordering.by(box => { count += 1; box.value })
  implicit private val ord: Ordering[Box[Int]] = genOrd

  private val sets = {
    val values = (1 to 20).map(Box(_))
    Seq[collection.SortedSet[Box[Int]]](
      values.to(immutable.SortedSet),
      values.to(mutable.SortedSet),
    )
  }

  @Test
  def min(): Unit = {
    for (set <- sets) {
      count = 0
      set.min(using ord)
      assert(count == 0)

      count = 0
      set.min(using ord.reverse)
      assert(count == 0)

      count = 0
      set.min(using genOrd)
      assert(count > 10)
    }
  }

  @Test
  def max(): Unit = {
    for (set <- sets) {
      count = 0
      set.max(using ord)
      assert(count == 0)

      count = 0
      set.max(using ord.reverse)
      assert(count == 0)

      count = 0
      set.max(using genOrd)
      assert(count > 10)
    }
  }

  @Test
  def min_max_differentOrdering(): Unit = {
    implicit val forward: Ordering[Int] = (x, y) => Ordering.Int.compare(x, y)

    val set = SortedSet(1, 2, 3)(using Ordering.Int.reverse)
    assertEquals(1, set.min)
    assertEquals(3, set.max)
    assertEquals(3, set.min(using set.ordering))
    assertEquals(1, set.max(using set.ordering))
  }
}

private object SortedSetTest {
  final case class Box[A](value: A)
}
