abstract class Foo[A] {
  def foo(x: A): Unit
}
abstract class Bar[A] extends Foo[A] {
  def foo(x: A with String): Unit = println(x.toUpperCase)
}
object Baz extends Bar[Int] // error overriding foo: incompatible type
                            // Scala 2 gives: object creation impossible. Missing implementation for `foo`

object Test {
  def main(args: Array[String]) = Baz.foo(42)
}
