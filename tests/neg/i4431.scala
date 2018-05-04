
object Foo {
  inline def h(inline f: Int => String): String = ~ '(f(42)) // error
}
