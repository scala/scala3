case class Foo(n: Int)

class Bar(n: Int):
  given foo: Foo = new Foo(n)

class InMethod:
  def wild(bar: Bar): Unit =
    import bar.*
    given foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def givenWild(bar: Bar): Unit =
    import bar.{ given, * }
    given foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def givn(bar: Bar): Unit =
    import bar.given
    given foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def givenFoo(bar: Bar): Unit =
    import bar.given Foo
    given foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def named(bar: Bar): Unit =
    import bar.foo
    given foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def namedGivenWild(bar: Bar, bar2: Bar): Unit =
    import bar.foo, bar2.{ given, * }
    assert(bar.foo eq summon[Foo])

  def givenWildNamed(bar: Bar, bar2: Bar): Unit =
    import bar2.{ given, * }, bar.foo
    assert(bar.foo eq summon[Foo])

  def namedWild(bar: Bar, bar2: Bar): Unit =
    import bar.foo, bar2.*
    assert(bar.foo eq summon[Foo])

  def wildNamed(bar: Bar, bar2: Bar): Unit =
    import bar2.*, bar.foo
    assert(bar.foo eq summon[Foo])

class InClassWild(bar: Bar):
  import bar.*
  given foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class InClassGivenWild(bar: Bar):
  import bar.{ given, * }
  given foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class InClassGiven(bar: Bar):
  import bar.given
  given foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class InClassGivenFoo(bar: Bar):
  import bar.given Foo
  given foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class InClassNamed(bar: Bar):
  import bar.foo
  given foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

object Test:
  def main(args: Array[String]): Unit =
    val bar = new Bar(1)
    val bar2 = new Bar(2)

    new InMethod().wild(bar)
    new InMethod().givenWild(bar)            // was: error
    new InMethod().givn(bar)                 // was: error
    new InMethod().givenFoo(bar)             // was: error
    new InMethod().named(bar)                // was: error

    new InMethod().namedWild(bar, bar2)
    new InMethod().wildNamed(bar, bar2)
    new InMethod().namedGivenWild(bar, bar2) // was: error
    new InMethod().givenWildNamed(bar, bar2)

    new InClassWild(bar)
    new InClassGivenWild(bar)                // was: error
    new InClassGiven(bar)                    // was: error
    new InClassGivenFoo(bar)                 // was: error
    new InClassNamed(bar)                    // was: error
