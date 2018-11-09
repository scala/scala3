sealed abstract class Base

sealed trait A extends Base

object A {

  case object Root extends Base

  def apply(param: String): A = {
      new A {}
  }
}

object ExhaustiveMatchWarning {

  def test: Unit = {
    val b: Base = A("blabla")
    b match {    // A.apply creates anonymous class <: Base
      case A.Root => println("Root")
      case path: A => println("Not root")
    }
  }
}