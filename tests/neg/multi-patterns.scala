object Test {
  val (a :: as), bs = List(1, 2, 3) // error // error
  val B @ List(), C: List[Int] = List() // error
}