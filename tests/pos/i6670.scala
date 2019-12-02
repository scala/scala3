case class X[A](
  foo: A,
  bar: String = ""
) {
  def copy[A](
    foo: A = foo,
    bar: String = bar
  ): X[A] = ???
}
object Test {
  val x = X(1)
  x.copy(bar = "BAZ")
}