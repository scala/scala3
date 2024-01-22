import scala.annotation.tailrec

object Foo:
  @tailrec
  def foo: Int = foo // warn: Infinite recursive call

object Bar:
  export Foo.foo // def foo here should not have `@tailrec`
