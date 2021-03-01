import language.reflectiveCalls

object Foo {
  val m: ({ def foo(str: String, int: Int): Int }){ def foo(arg: Int): Int } = ???
  val m1 = m
  val n = m1.foo(23)  // error
}