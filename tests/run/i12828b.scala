class Foo[A] {
  def foo(x: A): Unit = ()
}
class Bar[A] extends Foo[A] {
  def foo(x: A with String): Unit = println(x.toUpperCase)
}
object Baz extends Bar[Int] // was error: Baz inherits conflicting members, now like Scalac
                            // Scala 2 compiles and runs

object Test {
  def main(args: Array[String]) = Baz.foo(42)
}
