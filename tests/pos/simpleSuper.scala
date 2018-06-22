package foo

class A {
  def foo: Int = 1
}

trait B {
  def foo: Int = 2
}

class C extends A with B {
  override def foo: Int = super.foo + super[B].foo
}
