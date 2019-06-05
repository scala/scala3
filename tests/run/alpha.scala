import annotation.alpha

object Test extends App {
  def foo(x: Any): Int = 1
  @alpha("bar") def foo[A <: AnyRef](x: A) = 2

  assert(foo("a") == 2)
}
