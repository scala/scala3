class Test {
  import MyEnum.*

  def f(e: MyEnum) = e match {
    case ONE => println("one")
    case TWO => println("two")
    // missing case --> exhaustivity warning!
  }

  import MySecondEnum.*
  def g(e: MySecondEnum) = e match {
    case RED => println("red")
    // missing case --> exhaustivity warning!
  }
}
