
object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo: Baz).value)
    println((new Foo: Qux).value)
  }
}

class Foo extends Bar[Boolean](true) with Baz with Qux

class Bar[T](x: T) {
  def value: T = x
}

trait Baz {
  def value: Boolean
}

trait Qux {
  def value: Boolean
}
