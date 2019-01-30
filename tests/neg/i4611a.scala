// Don't qualify as SAM type because result type is an implicit function type
trait Foo {
  def foo(x: Int): given Int => Int
}

trait Bar[T] {
  def bar(x: Int): T
}

class Test {
  val good1 = new Foo {
    def foo(x: Int) = 1
  }

  val good2 = new Bar[given Int => Int] {
    def bar(x: Int) = 1
  }

  val bad1: Foo = (x: Int) => 1 // error
  val bad2: Bar[given Int => Int] = (x: Int) => 1 // error
}
