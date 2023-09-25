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

  def namedWild(bar: Bar, bar2: Bar): Unit =
    import bar.foo
    import bar2._
    assert(bar.foo eq implicitly[Foo])

  def wildNamed(bar: Bar, bar2: Bar): Unit =
    import bar2._
    import bar.foo
    assert(bar.foo eq implicitly[Foo])

class InClassWild(bar: Bar):
  import bar._
  implicit val foo: Foo = new Foo(2)
  assert(foo eq implicitly[Foo])

class InClassNamed(bar: Bar):
  import bar.foo
  implicit val foo: Foo = new Foo(2)
  assert(foo eq implicitly[Foo])

object Test:
  def main(args: Array[String]): Unit =
    val bar = new Bar(1)
    val bar2 = new Bar(2)

    new InMethod().wild(bar)            // was: error
    new InMethod().named(bar)           // was: error

    new InMethod().namedWild(bar, bar2) // was: error
    new InMethod().wildNamed(bar, bar2)

    new InClassWild(bar)                // was: error
    new InClassNamed(bar)               // was: error
