
class Test {

  import AbstractSealed.*

  def f(a: AbstractSealed) = a match {
    case A => println("A")
    case B => println("B")
    // missing case --> exhaustivity warning!
  }
}