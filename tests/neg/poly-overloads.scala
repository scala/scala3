class A {
  def foo1(x: Int): Int = x
  def foo1[T]: String => T = ???

  foo1("") // ok

  def foo2(x: Int): Int = x
  def foo2[T]: T => String = ???

  foo2(1): String // ok
  foo2("") // ok, unlike Scala 2

  def foo3(x: Any): Any = x
  def foo3[T <: Int]: T => T = x => x

  val a = foo3(1) // ok
  val b: Int = a // ok, unlike Scala 2 which prefers the first overload

  def foo4(x: Any): Any = x
  def foo4[T >: Int]: T => T = x => x

  val c = foo4(1) // error, unlike Scala 2 this is ambiguous
}
