//> using options -Wvalue-discard -Wnonunit-statement

class Assertion(assert: => Any):
  def shouldPass(): Assertion = ???

def test: Unit =
  1 + 1: Unit
  (1 + 1): Unit
  val x: Unit = 1 + 1 // warn
  1 + 1 // warn
  val y: Int = 1 + 1

  new Assertion("").shouldPass(): Unit
  (new Assertion("").shouldPass()): Unit
  val x1: Unit = new Assertion("another").shouldPass() // warn (enabled by -Wvalue-discard)
  new Assertion("yet another").shouldPass() // warn (enabled by -Wnonunit-statement)
  val y1: Assertion = new Assertion("another other").shouldPass()

  ()
