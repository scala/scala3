class A
trait Bar[X] {
  // same for `val foo: X = ???`
  def foo: X = ???
}
// same for `class Foo(...)...`
trait Foo(val a: A) extends Bar[a.type] { // error
  val same: a.type = foo
}
