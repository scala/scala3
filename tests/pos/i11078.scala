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

trait FooCreatorWrapper {
  type A <: Foo[A]
  def fooCreator: FooCreator[A]
}

sealed trait Bar
object Bar {
  case class Baz(wrapper: FooCreatorWrapper) extends Bar
}

def process(bar: Bar): FooWrapper = bar match {
  case Bar.Baz(wrapper) => FooWrapper(wrapper.fooCreator.createFoo())
}