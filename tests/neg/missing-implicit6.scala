trait Foo {
  type Out <: { type Out }
}

trait Bar {
  type Out
}

object instances {
  given foo: Foo:
    type Out = Bar

  given bar: Bar:
    type Out = Int
}

object Test {
  object Ops {
    extension (using foo: Foo, bar: foo.Out)(i: Int)
      def xxx = ???

    extension (using foo: Foo, fooOut: foo.Out)(x: fooOut.Out)
      def yyy = ???

    extension (using foo: Foo)(i: Int)(using fooOut: foo.Out)
      def zzz = ???
  }

  locally {
    import instances.given

    "a".xxx // error, no suggested import
    123.xxx // error, suggested import
    123.yyy // error, suggested import
  }

  locally {
    import instances.foo
    123.xxx // error, no suggested import
    123.yyy // error, no suggested import
    123.zzz // error, suggested import even though there's no instance of Bar in scope
  }
}
