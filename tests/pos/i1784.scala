object Test {
  implicit class Foo(sc: StringContext) {
    object q {
      def unapply(args: Any*): Option[(Any, Any)] =
        Some((sc.parts(0), sc.parts(1)))
    }
  }

  def f(defn: Any): Any = {
    val q"class $name extends $parent" =  defn
    println(name)
    println(parent)
  }
}