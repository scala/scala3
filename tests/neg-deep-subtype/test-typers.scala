object typers {

  class A(x: Int) {
    val x: String = "a"  // error: double def

    { val y: String = ""
      val y: Int = 0     // error: double def
      y
    }
  }

  class B { self =>      // error: double def
    def self: Int = 0
    def self(x: Int): Int = x
  }

  class C {
    val x: Int
    val x: String        // error: double def
    val y: Int
    def y: String        // error: double def
    val z: Int
    def z(): String      // error: double def

    def f(x: Any) = ()   // OK!
    def f(x: AnyRef): AnyRef

    def g(x: Object): Unit
    def g[T](x: T): T = x  // OK!
  }

  type L[X] = scala.collection.immutable.List[X]
  type M[X, Y] <: scala.collection.immutable.Map[X, Y] // old-error: only classes can have declared but undefined members

  object hk {
    def f(x: L)  // error: missing type parameter
    : M =        // error: missing type parameter
    ??? : M      // error: missing type parameter
  }

  object returns {

    def foo(x: Int) = {
      return 3          // error: has return; needs result type
    }

    return 4            // error: return outside method definition
  }

  object cyclic {
    def factorial(acc: Int, n: Int) =
      if (n == 0) acc
      else factorial(acc * n, n - 1)    // error: cyclic reference

    def foo(x: Int) = x
    def foo() = foo(1)                  // error: cyclic reference

  }

  object tries {

    val x = try {
      "abc"
    } catch {
      case ex: String => // does not work yet. We should detect that the test is non-sensical, but don't.
        123
    }
  }
}
