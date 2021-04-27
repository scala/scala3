def test: Unit =
  IArray(2, 1, 3).toSeq
  IArray(2, 1, 3).toSeq.toList
  IArray(2, 1, 3).toSeq.updated(1, 2)
  IArray(2, 1, 3).toSeq.updated(1, "a")
  IArray(2, 1, 3).toSeq.sorted
  IArray(2, 1, 3).toSeq.appended("b")
  IArray(2, 1, 3).toSeq.prepended("b")
  IArray(2, 1, 3).toSeq.appendedAll(IArray(4,5, "a").toSeq)
  IArray(2, 1, 3).toSeq.prependedAll(IArray(4,5, "a").toSeq)
  val x1: Seq[Int] = Array(1, 2).toSeq
  val x2: Seq[Int] = IArray(1, 2).toSeq
