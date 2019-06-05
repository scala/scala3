import annotation.alpha

object Test {

  def foo() = 1

  @alpha("bar") def foo(x: Int) = 2
}
