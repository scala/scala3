//> using options  -Wimplausible-patterns
object UnitTest {
  def foo(m: Unit) = m match {
    case runtime.BoxedUnit.UNIT => println("ok") // warn
  }
  foo(())
}

