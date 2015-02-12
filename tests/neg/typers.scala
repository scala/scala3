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

    def f(x: Any) = ()   // error: double def
    def f(x: AnyRef): AnyRef

    def g(x: Object): Unit
    def g[T](x: T): T = x  // error: double def
  }




  object returns {

    def foo(x: Int) = {   // error: has return; needs result type
      return 3
    }

    return 4            // error: return outside method definition
  }

  object cyclic {
    def factorial(acc: Int, n: Int) =
      if (n == 0) acc
      else factorial(acc * n, n - 1)    // error: cyclic reference

    def foo(x: Int) = x                 // error: cyclic reference
    def foo() = foo(1)

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
