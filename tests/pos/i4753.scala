class A

trait Foo {
  def foo: given A => Int
}

class Test {
  new FooI{}
}

class FooI extends Foo {
  def foo: given A => Int = 3
}