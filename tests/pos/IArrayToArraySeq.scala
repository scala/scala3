def test: Unit =
  IArray(2, 1, 3).toList
  IArray(2, 1, 3).toSeq
  IArray(2, 1, 3).updated(1, 2)
  IArray(2, 1, 3).updated(1, "a")
  IArray(2, 1, 3).sorted
  IArray(2, 1, 3).appended("b")
  IArray(2, 1, 3).prepended("b")
  IArray(2, 1, 3).appendedAll(IArray(4,5, "a"))
  IArray(2, 1, 3).prependedAll(IArray(4,5, "a"))
  val x1: Seq[Int] = Array(1, 2)
  val x2: Seq[Int] = IArray(1, 2)
