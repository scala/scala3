// Test that constructors have a non-nullab.e return type.

class Foo {
  val x: java.lang.String = new java.lang.String()
  val y: java.util.Date = new java.util.Date()
  val v = new java.util.Vector[String](null /*stands for Collection<? extends E>*/)
}
