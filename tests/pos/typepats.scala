object Test {
  val xs: Any = List(1, 2, 3)
  xs match {
    case xs: List[type T] =>
  }
  trait I[T]
  class C[T] extends I[T]
  val y: I[Int] = new C[Int]
  y match {
    case _: C[type T] =>
      val x: T = 3
  }

  import collection.immutable.SortedSet
  val s: Set[Int] = SortedSet(1, 2, 3)
  s match {
    case _: SortedSet[type T] =>
      val x: T = 3
    case _ =>
  }
}