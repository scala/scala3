class A
class B

trait Foo {
  def foo: implicit A => implicit B => Int
}

class Foo1 extends Foo {
  def foo: implicit A => implicit B => Int = 1
}

class Foo2 extends Foo1 {
  override def foo: implicit A => implicit B => Int = 2
}

trait Foo3 extends Foo {
  override def foo: implicit A => implicit B => Int = 3
}

class Bar[T] {
  def bar: implicit A => T = null.asInstanceOf[T]
}

class Bar1 extends Bar[implicit B => Int] {
  override def bar: implicit A => implicit B => Int = 1
}

object Test {
  def testFoo() = {
    implicit val a = new A
    implicit val b = new B
    assert((new Foo1).foo == 1)
    assert((new Foo2).foo == 2)
    assert(new Foo3{}.foo == 3)
  }

  def testBar() = {
    implicit val a = new A
    implicit val b = new B
    assert((new Bar).bar == null)
    assert((new Bar1).bar == 1)
  }

  def main(args: Array[String]): Unit = {
    testFoo()
    testBar()
  }
}
