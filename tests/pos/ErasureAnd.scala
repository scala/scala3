import scala.annotation.tailrec
trait A { self: B =>
  @tailrec
  private def foo(arg1: Int, arg2: Int): Int = {
    def bar = this.foo(arg1, arg2)
    foo(arg1, arg2)
  }
  def foo(arg: Int) = arg
}

class B extends A{}
