abstract class Base {
  def foo(): Unit
  foo()
}

object O extends Base {              // error
  val msg: String = "hello"

  class Inner {
    println(msg)
  }

  def foo() = new Inner
}

@main
def Test = O