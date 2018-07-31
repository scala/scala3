class A

trait Foo {
  def foo: implicit A => Int
}

class Test {
  new FooI{}
}

class FooI extends Foo {
  def foo: implicit A => Int = 3
}