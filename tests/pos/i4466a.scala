class Foo {
  def apply(bar: Bar[Int]): Unit = ()
  def apply(i: Int): Unit = ()
}

class Bar[X](x: X)

class Main {
  val foo = new Foo
  foo(new Bar(0))
}