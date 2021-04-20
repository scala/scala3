class Foo[A, B]()

type FooSnd[X] = X match
  case Foo[_, b] => b

trait Bar[A]:
  def bar(h: FooSnd[A]): Int

val foo: Bar[Foo[String, Int]] = new Bar[Foo[String, Int]]:
  def bar(h: FooSnd[Foo[String, Int]]) = h
