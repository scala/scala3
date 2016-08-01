object Test {

  val x: List[String] = List(1) // error: found Int(1), expected: String

  val y: List[List[String]] = List(List(1)) // error: found Int(1), expected: String

  val z: (List[String], List[Int]) = (List(1), List("a")) // error: found Int(1), expected: String // error: found String(a), expected: Int
}
