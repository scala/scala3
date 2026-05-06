package tests.warn.i22298_joint;

object Tester {

  val abstractSealed: AbstractSealed = new A()

  abstractSealed match { // warn
    case _: B => ()
  }
}