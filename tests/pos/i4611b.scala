class A
class B
class C

trait Foo {
  def foo: implicit A => implicit B => C
}

class FooImpl extends Foo {
  def foo: implicit A => implicit B => C = new C
}

trait Bar extends Foo {
  def bar: implicit A => implicit B => C = foo
}

class BarImpl {
  def foo = new C
}
