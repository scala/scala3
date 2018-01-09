object Test {
  def foo[T <: Singleton](x: T): T = x
  def bar[T <: Int with Singleton](x: T): T = x

  val a: 1 = foo(1)
  val b: 1 = bar(1)
}
