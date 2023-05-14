import annotation.targetName

object Test extends App {
  def foo(x: Any): Int = 1
  @targetName("bar") def foo[A <: AnyRef](x: A) = 2

  assert(foo("a") == 2)
}
