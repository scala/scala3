import annotation.targetName

object Test {

  def foo() = 1

  @targetName("bar") def foo(x: Int) = 2
}
