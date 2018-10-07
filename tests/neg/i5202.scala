object Test {
  val f: (Int => Int) | (String => Int) = (a: Int) => a + 3

  f.apply(5) // error - found: Int expected: Int & String
  f("c")     // error - found: String expected: Int & String
}