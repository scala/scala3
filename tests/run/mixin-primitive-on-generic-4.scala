
object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo: Baz).value1.v)
    println((new Foo: Baz).value2().v)
  }
}

class Foo extends Bar[VBoolean](new VBoolean(true)) with Baz

class Bar[T](x: T) {
  def value1: T = x
  def value2(): T = x
}

trait Baz {
  def value1: VBoolean
  def value2(): VBoolean
}

class VBoolean(val v: Boolean) extends AnyVal
