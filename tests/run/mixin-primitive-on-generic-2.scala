
object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo: Bar[Boolean]).value1)
    println((new Foo: Bar[Boolean]).value2())
  }
}

class Foo extends Baz with Bar[Boolean]

trait Bar[T] {
  def value1: T
  def value2(): T
}

class Baz {
  def value1: Boolean = true
  def value2(): Boolean = true
}
