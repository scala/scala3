
object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo: Bar[VBoolean]).value1.v)
    println((new Foo: Bar[VBoolean]).value2().v)
  }
}

class Foo extends Baz with Bar[VBoolean]

trait Bar[T] {
  def value1: T
  def value2(): T
}

class Baz {
  def value1: VBoolean = new VBoolean(true)
  def value2(): VBoolean = new VBoolean(true)
}

class VBoolean(val v: Boolean) extends AnyVal
