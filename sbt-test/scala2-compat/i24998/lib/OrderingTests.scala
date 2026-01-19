object OrderingTests {
  final case class Pair(a: Int, b: Int)

  def apply(): Unit = {
    val byA = Ordering.by[Pair, Int](_.a)
    val byB = Ordering.by[Pair, Int](_.b)
    val orElseOrd = byA.orElse(byB)
    assert(orElseOrd.compare(Pair(1, 2), Pair(1, 3)) < 0)
    assert(orElseOrd.compare(Pair(2, 0), Pair(1, 99)) > 0)

    val orElseByOrd = byA.orElseBy[Int](_.b)
    assert(orElseByOrd.compare(Pair(1, 2), Pair(1, 3)) < 0) // error
    assert(orElseByOrd.compare(Pair(2, 0), Pair(1, 99)) > 0) // error
  }
}
