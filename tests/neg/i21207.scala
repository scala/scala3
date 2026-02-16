
abstract class P {
  def foo1(): Int = 1
  def foo2(x: Int): Int = 22
  def g4(s: String): String = s * 4
}

class C extends P {
  def foo1(x: Int): Int = 11
  def foo2(): Int = 2

  def foo3(): Int = 3
  def foo3(x: Int): Int = 33

  def g4(): Int = 4
}

object Test {
  val c = new C
  println(c.foo1) // error was omitted because of nullary fallback during ambiguous overload resolution
  println(c.foo2) // error, add parens
  println(c.foo3) // error
  println(c.g4) // error
  val s: String = c.g4 // error missing arg, expected type picks method
  println(s)
}

/* Scala 2 warns for all three selections:
warning: Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method foo1,
or remove the empty argument list from its definition (Java-defined methods are exempt).
In Scala 3, an unapplied method like this will be eta-expanded into a function. [quickfixable]
*/
