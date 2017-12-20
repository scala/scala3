object Test {
  val x: iDontExist = 1 // error: not found: type iDontExist

  val y = x.asInstanceOf[Int] // No error reported (was: value asInstanceOf does not take type parameters)

  val a: iDontExist | Int = 1 // error: not found: type iDontExist
  val a2 = a.isInstanceOf[Int] // No error (used to crash)

  val b: iDontExist & Int = 1 // error: not found: type iDontExist
  val b2 = a.isInstanceOf[Int] // No error (worked before too)
}
