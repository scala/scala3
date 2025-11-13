abstract class Base {
  val msg: String = "hello"
  def foo(): Unit
  foo()
}

object O extends Base {              // error

  class Inner {
    println(msg) // warn
  }

  def foo() = new Inner // warn
}

@main
def Test = O
