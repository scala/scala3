@main def Test =
  val l1: List[Int] = (1, 2, 3).toList
  val l2: List[Int | String] = (1, "foo", 3).toList
  val l3: List[Int | String | 1] = (1, "foo", 1).toList
  println(l1)
  println(l2)
  println(l3)
