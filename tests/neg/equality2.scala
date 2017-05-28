object equality2 {
  class A

  val x: Int = 3
  x == new A // error: cannot compare
  new A == x // error: cannot compare
  1 == new A // error: cannot compare
  new A == 1 // error: cannot compare

  true == new A // error: cannot compare
  new A == true // error: cannot compare
}
