final class X
final class Y

object Test3 {
  type Bar[A] = A match {
    case X => String
    case Y => Int
  }

  trait XX {
    type Foo

    val a: Bar[X & Foo] = "hello"
    val b: Bar[Y & Foo] = 1

    def apply(fa: Bar[X & Foo]): Bar[Y & Foo]

    def boom: Int = apply(a)
  }

  trait YY extends XX {
    type Foo = X & Y

    def apply(fa: Bar[X & Foo]): Bar[Y & Foo] = fa // error
        // overriding method apply in trait XX of type (fa: String): Int;
        //   method apply of type (fa: String): String has incompatible type
  }
  (new YY {}).boom
}

object Test4 {
  type Bar[A] = A match {
    case X => String
    case Y => Int
  }

  trait XX {
    type Foo
    type FooAlias = Foo

    val a: Bar[X & FooAlias] = "hello"
    val b: Bar[Y & FooAlias] = 1

    def apply(fa: Bar[X & FooAlias]): Bar[Y & FooAlias]

    def boom: Int = apply(a)
  }

  trait YY extends XX {
    type Foo = X & Y

    def apply(fa: Bar[X & FooAlias]): Bar[Y & FooAlias] = fa // error
        // overriding method apply in trait XX of type (fa: String): Int;
        //   method apply of type (fa: String): String has incompatible type
  }
  (new YY {}).boom
}
