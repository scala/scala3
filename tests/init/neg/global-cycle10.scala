abstract class Base {
  val msg: String = "hello"
  def foo(): Unit
  foo()
}

object O extends Base {              // error

  class Inner {
    println(msg)
  }

  def foo() = new Inner
}

@main
def Test = O
