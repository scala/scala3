class IOCapability

object Test {
  // Forgot to mark `ev` implicit!
  def doSideEffects(x: Int)(ev: IOCapability) = {
    println("x: " + x)
  }

  implicit val cap: IOCapability = new IOCapability

  2 // error: pure expression does nothing in statement position

  doSideEffects(1) // error: pure expression does nothing in statement position
}
