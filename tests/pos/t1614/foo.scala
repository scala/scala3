// foo.scala
trait Foo {
  def foo(arg: List[?]): Unit
}
trait FooImpl extends Foo {
    def foo(arg: List[?]): Unit = {}
}
trait AbstractOverrideFoo extends Foo {
    abstract override def foo(arg: List[?]): Unit = {
        super.foo(arg)
    }
}
