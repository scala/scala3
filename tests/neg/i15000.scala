object ExtensionMethodReproduction {
    import scala.language.implicitConversions
    class StringExtensions {
      def apply(x: Int, y: String): String = "foo"
      def apply(x: Int, y: Int): String = "foo"
    }
    implicit def stringExtensions(m: String): StringExtensions = new StringExtensions()

    class C
    object c extends C

    extension (c: C)
      def apply(x: Int, y: String): String = "foo"
      def apply(x: Int, y: Int): String = "foo"

    val str = "POST"
    val x: Int = 1
    val fooY: String = "foo"
    str(x, barY) // error
    c(x, barY) // error // error
  }