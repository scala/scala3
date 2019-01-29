class A

trait Foo {
  def foo: A |=> Int
}

class Test {
  println(new FooI{})
}

class FooI extends Foo {
  def foo: A |=> Int = 3
}