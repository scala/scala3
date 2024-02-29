case class Foo(n: Int)

class OldBar(n: Int):
  implicit val foo: Foo = new Foo(n)

class NewBar(n: Int):
  given foo: Foo = new Foo(n)

class OldInMethod:
  def wild(bar: OldBar): Unit =
    import bar.*
    given foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def named(bar: OldBar): Unit =
    import bar.foo
    given foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def namedWild(bar: OldBar, bar2: NewBar): Unit =
    import bar.foo, bar2.*
    assert(bar.foo eq summon[Foo])

  def wildNamed(bar: OldBar, bar2: NewBar): Unit =
    import bar2.*, bar.foo
    assert(bar.foo eq summon[Foo])

  def namedGivenWild(bar: OldBar, bar2: NewBar): Unit =
    import bar.foo
    import bar2.{ given, * }
    assert(bar.foo eq summon[Foo])

  def givenWildNamed(bar: OldBar, bar2: NewBar): Unit =
    import bar2.{ given, * }, bar.foo
    assert(bar.foo eq summon[Foo])

class OldInClassWild(bar: OldBar):
  import bar.*
  given foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class OldInClassNamed(bar: OldBar):
  import bar.foo
  given foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])


class NewInMethod:
  def givenWild(bar: NewBar): Unit =
    import bar.{ given, * }
    implicit val foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def wild(bar: NewBar): Unit =
    import bar.*
    implicit val foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def givn(bar: NewBar): Unit =
    import bar.given
    implicit val foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def givenFoo(bar: NewBar): Unit =
    import bar.given Foo
    implicit val foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def named(bar: NewBar): Unit =
    import bar.foo
    implicit val foo: Foo = new Foo(2)
    assert(foo eq summon[Foo])

  def namedWild(bar: NewBar, bar2: OldBar): Unit =
    import bar.foo, bar2.*
    assert(bar.foo eq summon[Foo])

  def wildNamed(bar: NewBar, bar2: OldBar): Unit =
    import bar2.*, bar.foo
    assert(bar.foo eq summon[Foo])

class NewInClassGivenWild(bar: NewBar):
  import bar.{ given, * }
  implicit val foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class NewInClassWild(bar: NewBar):
  import bar.*
  implicit val foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class NewInClassGiven(bar: NewBar):
  import bar.given
  implicit val foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class NewInClassGivenFoo(bar: NewBar):
  import bar.given Foo
  implicit val foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])

class NewInClassNamed(bar: NewBar):
  import bar.foo
  implicit val foo: Foo = new Foo(2)
  assert(foo eq summon[Foo])


object Test:
  def main(args: Array[String]): Unit =
    val oldBar = new OldBar(1)
    val newBar = new NewBar(1)
    val oldBar2 = new OldBar(2)
    val newBar2 = new NewBar(2)


    new OldInMethod().wild(oldBar)                    // was: error
    new OldInMethod().named(oldBar)                   // was: error

    new OldInMethod().namedWild(oldBar, newBar2)
    new OldInMethod().wildNamed(oldBar, newBar2)
    new OldInMethod().namedGivenWild(oldBar, newBar2) // was: error
    new OldInMethod().givenWildNamed(oldBar, newBar2)

    new OldInClassWild(oldBar)                        // was: error
    new OldInClassNamed(oldBar)                       // was: error


    new NewInMethod().wild(newBar)
    new NewInMethod().givenWild(newBar)               // was: error
    new NewInMethod().givn(newBar)                    // was: error
    new NewInMethod().givenFoo(newBar)                // was: error
    new NewInMethod().named(newBar)                   // was: error

    new NewInMethod().namedWild(newBar, oldBar2)      // was: error
    new NewInMethod().wildNamed(newBar, oldBar2)

    new NewInClassWild(newBar)
    new NewInClassGivenWild(newBar)                   // was: error
    new NewInClassGiven(newBar)                       // was: error
    new NewInClassGivenFoo(newBar)                    // was: error
    new NewInClassNamed(newBar)                       // was: error
