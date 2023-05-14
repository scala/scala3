object Test {
  implicit class Foo(sc: StringContext) {
    object q {
      def apply(arg: Any*): Int = 3
    }
  }

  def f = {
    val _parent = 3
    q"val hello = $_parent"
    q"class $_" // error // error
  }
}
