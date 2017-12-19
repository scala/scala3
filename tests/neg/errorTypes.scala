object Test {
  val x: iDontExist = 1 // error: not found: type iDontExist

  val y = x.asInstanceOf[Int] // No error reported (was: value asInstanceOf does not take type parameters)
}
