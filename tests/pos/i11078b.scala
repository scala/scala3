class Test {
  trait Foo[A <: Foo[A]]

  trait FooWrapper:
    self =>
    type A <: Foo[A]
    def doThing(foo: FooWrapper): FooWrapper { type A = self.A } = ???
  end FooWrapper

  val foos: scala.Seq[FooWrapper] = ???
  val newFoo = foos.foldLeft(??? : FooWrapper)((topFoo, foo) => topFoo.doThing(foo))
}
