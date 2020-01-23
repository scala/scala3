class A

trait Foo {
  def foo: A ?=> Int
}

class Test {
  new FooI{}
}

class FooI extends Foo {
  def foo: A ?=> Int = 3
}