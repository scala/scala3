import scala.annotation.tailrec

object Foo:
  @tailrec
  def foo(n: Int): Int =
    if n == 0 then 0
    else foo(n-1)

object Bar:
  export Foo.foo // def foo here should not have `@tailrec`
