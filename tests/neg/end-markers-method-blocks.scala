import scala.language.experimental.methodBlockEndMarkers

object Test:
  def test(name: String)(body: => Unit): Unit =
    println(s"Running test: $name")
    body

  def foo(arg: Int)(block: => Unit): Unit = block

  def bar(x: Int): Unit = ()

  def main(args: Array[String]): Unit =
    // Misaligned end marker example from SIP
    def testFunction = bar:
      //do something
      println("inside bar")
    end bar  // error: misaligned end marker

    val baz = foo(42):
      //do something
    end foo // error: misaligned end marker
    end baz

    // Wrong method name examples
    test("my test"):
      val x = 1
      assert(x > 0)
    end wrong  // error

    foo(42):
      val result = 42 * 2
      println(s"Result: $result")
    end bar    // error

    // Wrong end marker for nested blocks
    test("nested test"):
      val x = 1
      val y = 2
      val z = x + y
      assert(z == 3)
    end foo   // error

    // Wrong end marker for complex test
    test("complex test"):
      val x = 1
      val y = 2
      val z = x + y
      assert(z == 3)
      println(s"Result: $z")
    end bar   // error

    // Wrong end marker for lambda
    val elements = List(1, 2, 3)
    elements.foreach:
      elem =>
        println(s"Element: $elem")
    end test  // error

    // Wrong end marker for explicit apply
    object Foo:
      def apply(block: => Unit): Unit = block

    Foo.apply:
      println("Explicit apply call")
    end Foo  // error

    // Wrong end marker for implicit apply
    Foo:
      println("Implicit apply call")
    end apply  // error

    // Wrong end marker for class instance
    class Bar:
      def apply(block: => Unit): Unit = block

    val bar = new Bar
    bar:
      println("Class instance apply call")
    end Bar  // error

    // Wrong end marker for curried method
    def curriedFoo(bar: String)(baz: String): Unit = 
      println(s"$bar and $baz")

    curriedFoo("abc"):
      "xyz"
    end curriedBar  // error

    // Wrong end marker for extension method
    extension (s: String)
      def withPrefix(prefix: String): String = s"$prefix$s"

    "world".withPrefix:
      "hello"
    end withSuffix  // error
