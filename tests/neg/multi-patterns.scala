object Test {
  val (a :: as), bs = List(1, 2, 3) // error
  val B as List(), C: List[Int] = List() // error
}