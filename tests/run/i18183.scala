case class Foo(n: Int)

class Bar(n: Int):
  implicit val foo: Foo = new Foo(n)

class InMethod:
  def wild(bar: Bar): Unit =
    import bar._
    implicit val foo: Foo = new Foo(2)
    assert(foo eq implicitly[Foo])

  def named(bar: Bar): Unit =
    import bar.foo
    implicit val foo: Foo = new Foo(2)
    assert(foo eq implicitly[Foo])

  def namedWild(bar: Bar): Unit =
    val bar2 = new Bar(2)
    import bar.foo
    import bar2._
    assert(bar.foo eq implicitly[Foo])

  def wildNamed(bar: Bar): Unit =
    val bar2 = new Bar(2)
    import bar2._
    import bar.foo
    assert(bar.foo eq implicitly[Foo])

class InClass(bar: Bar):
  import bar._
  implicit val foo: Foo = new Foo(2)
  assert(foo eq implicitly[Foo])

object Test:
  def main(args: Array[String]): Unit =
    val bar = new Bar(1)
    new InMethod().wild(bar)
    new InMethod().named(bar)
    new InMethod().namedWild(bar)
    new InMethod().wildNamed(bar)
    new InClass(bar)
