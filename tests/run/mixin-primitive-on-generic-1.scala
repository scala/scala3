
object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo: Baz).value1)
    println((new Foo: Baz).value2())
  }
}

class Foo extends Bar[Boolean](true) with Baz

class Bar[T](x: T) {
  def value1: T = x
  def value2(): T = x
}

trait Baz {
  def value1: Boolean
  def value2(): Boolean
}
