
class S {
  def foo[T](x: T): T = x
  // Check that the type argument to `foo` is inferred to be
  // `String|Null`: i.e. it isn't collapsed.
  val x = foo(if (1 == 2) "hello" else null)
  val y: String|Null = x
}
