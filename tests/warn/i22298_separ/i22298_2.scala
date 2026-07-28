package tests.warn.i22298_separ;

object Tester {

  val abstractSealed: AbstractSealed_1 = new A()

  abstractSealed match { // warn
    case _: B => ()
  }
}