    class Foo[T] {
      def zero: T = ???
      def one(): T = ???
      val two: T = ???
    }

    object Test {
      def foo[T](x: T): Foo[T] = new Foo

      val b: 1 = foo(1).one() // OK
      val a: 1 = foo(1).zero // Fails: Found: Int, required: Int(1)
      val c: 1 = foo(1).two  // Fails: Found: Int, required: Int(1)
    }
