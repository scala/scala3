trait Foo[A <: Foo[A]]
trait FooCreator[A <: Foo[A]] {
  def createFoo(): A
}

trait FooWrapper {
  type A <: Foo[A]
  def foo: A
}

object FooWrapper {
  def apply[A0 <: Foo[A0]](toWrap: A0): FooWrapper { type A = A0 } = new FooWrapper {
    type A = A0
    def foo: A0 = toWrap
  }
}

def error(fooWrapper: FooWrapper, processor: [A <: Foo[A]] => A => A): FooWrapper =
  FooWrapper(processor(fooWrapper.foo))
