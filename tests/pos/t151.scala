abstract class Foo {
  type T;
  def foo(a: T): Int = 0;
  val foo: Foo = ???;
  def a: foo.T = a;
}
