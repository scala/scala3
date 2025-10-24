import scala.language.experimental.endMarkersForMethodBlocks

object Test:
  def test(name: String)(body: => Unit): Unit =
    println(s"Running test: $name")
    body

  def foo(arg: Int)(block: => Unit): Unit = block

  def main(args: Array[String]): Unit =
    // Basic examples from SIP
    test("my test"):
      val x = 1
      assert(x > 0)
    end test

    foo(42):
      val result = 42 * 2
      println(s"Result: $result")
    end foo

    // Nested blocks example from SIP
    test("very long test"):
      locally:
        val setup = "setup"
        println(setup)
      end locally
      // assertions...
      assert(true)
    end test

    // Lambda with colon syntax example from SIP
    val elements = List(1, 2, 3)
    elements.foreach:
      elem =>
        println(s"Element: $elem")
    end foreach

    // Explicit apply method example from SIP
    object Foo:
      def apply(block: => Unit): Unit = block

    Foo.apply:
      println("Explicit apply call")
    end apply

    // Implicit apply method example from SIP
    Foo:
      println("Implicit apply call")
    end Foo

    // Class with apply method example from SIP
    class Bar:
      def apply(block: => Unit): Unit = block

    val bar = new Bar
    bar:
      println("Class instance apply call")
    end bar

    // Curried method example from SIP
    def curriedFoo(bar: String)(baz: String): Unit = 
      println(s"$bar and $baz")

    curriedFoo("abc"):
      "xyz"
    end curriedFoo

    // Curried class example from SIP
    class CurriedFoo(bar: String):
      def apply(baz: String): Unit = 
        println(s"$bar and $baz")

    def createCurriedFoo(bar: String) = CurriedFoo(bar)
    createCurriedFoo("abc"):
      "xyz"
    end createCurriedFoo

    // Extension method example
    extension (s: String)
      def withPrefix(prefix: String): String = s"$prefix$s"

    "world".withPrefix:
      "hello"
    end withPrefix
