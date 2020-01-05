object Test {
  val l1 = List(Predef.identity[Int](_))
  val lc1: List[Int => Int] = l1

  val l2 = List(Predef.identity[Int](_))
  val lc2: List[Int => Int] = l2
}