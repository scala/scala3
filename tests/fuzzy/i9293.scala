class Foo[X] {
  def foo: X[X] = foo(null)
}
object Foo {
  implicit def baz[X](arg: X): Foo[X] = ???
}
