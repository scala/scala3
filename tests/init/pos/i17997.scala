abstract class FunSuite:
  def foo(): Unit = println("FunSuite")

  foo()

trait MySelfType

trait MyTrait extends FunSuite { this: MySelfType =>
}

abstract class MyAbstractClass extends FunSuite { this: MySelfType =>

  override def foo() = {
    println("MyAbstractClass")
    super.foo()
  }
}

final class MyFinalClass extends MyAbstractClass with MyTrait with MySelfType:
  val n: Int = 100
