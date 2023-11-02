//> using options -Xfatal-warnings

class IOCapability

object Test {
  "" // warn: pure expression does nothing in statement position

  locally {
    "" // warn: pure expression does nothing in statement position

    println("")

    42 // warn: pure expression does nothing in statement position

    ((x: Int) => println("hi")) // warn: pure expression does nothing in statement position

    ()
  }

  // Forgot to mark `ev` implicit!
  def doSideEffects(x: Int)(ev: IOCapability) = {
    println("x: " + x)
  }

  implicit val cap: IOCapability = new IOCapability

  2 // warn: pure expression does nothing in statement position

  doSideEffects(1) // error: pure expression does nothing in statement position

  val broken = new IDontExist("") // error
  broken.foo // no extra error, and no pure expression warning
  broken.foo() // same
}
// nopos-error: No warnings can be incurred under -Werror.