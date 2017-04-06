trait A {
  private type Foo = Int

  def foo: Foo = 1
}
class B extends A {
  foo
}
