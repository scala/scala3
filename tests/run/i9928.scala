trait Magic[F]:
  extension (x: Int) def read: F

trait LowPrio:
  given Magic[String]:
    extension(x: Int) def read: String =
      println("In string")
      s"$x"

object test1:
  object Magic extends LowPrio

  opaque type Foo = String
  object Foo extends LowPrio:
    import Magic.given
    def apply(s: String): Foo = s

    given Magic[Foo]:
      extension (x: Int) def read: Foo =
        println("In foo")
        Foo(s"$x")

    def test: Unit =
      (3.read: Foo)

object test2:
  object Magic extends LowPrio:
    given Magic[Foo]:
      extension (x: Int) def read: Foo =
        println("In foo")
        Foo(s"$x")

  opaque type Foo = String
  object Foo extends LowPrio:
    import Magic.given
    def apply(s: String): Foo = s

    def test: Unit =
      (3.read: Foo)


@main def Test =
  test1.Foo.test
  test2.Foo.test
