trait Magic[F] with
  extension (x: Int) def read: F

trait LowPrio with
  given Magic[String] with
    extension(x: Int) def read: String =
      println("In string")
      s"$x"

object test1 with
  object Magic extends LowPrio

  opaque type Foo = String
  object Foo extends LowPrio with
    import Magic.given
    def apply(s: String): Foo = s

    given Magic[Foo] with
      extension (x: Int) def read: Foo =
        println("In foo")
        Foo(s"$x")

    def test: Unit =
      (3.read: Foo)

object test2 with
  object Magic extends LowPrio with
    given Magic[Foo] with
      extension (x: Int) def read: Foo =
        println("In foo")
        Foo(s"$x")

  opaque type Foo = String
  object Foo extends LowPrio with
    import Magic.given
    def apply(s: String): Foo = s

    def test: Unit =
      (3.read: Foo)


@main def Test =
  test1.Foo.test
  test2.Foo.test
