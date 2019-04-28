object Test {
  val (a :: as), bs = List(1, 2, 3) // error
  val B @ List(), C: List[Int] = List() // error
}