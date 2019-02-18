class IOCapability

object Test {
  "" // error: pure expression does nothing in statement position

  locally {
    "" // error: pure expression does nothing in statement position

    println("")

    42 // error: pure expression does nothing in statement position

    ((x: Int) => println("hi")) // error: pure expression does nothing in statement position

    ()
  }

  // Forgot to mark `ev` implicit!
  def doSideEffects(x: Int)(ev: IOCapability) = {
    println("x: " + x)
  }

  implicit val cap: IOCapability = new IOCapability

  2 // error: pure expression does nothing in statement position

  doSideEffects(1) // error: pure expression does nothing in statement position

  val broken = new IDontExist("") // error // error
  broken.foo // no extra error, and no pure expression warning
  broken.foo() // same
}
