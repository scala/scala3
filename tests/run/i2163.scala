class Base(f: Int => Int) {
  def result = f(3)
}

class Child(x: Int) extends Base(y => x + y)

class Outer(z: Int) {
  class Base(f: Int => Int) {
    def result = f(3)
  }

  class Child(x: Int) extends Base(y => x + y + z)
}

object Test {
  def main(args: Array[String]): Unit = {
    assert(new Child(4).result == 7)
    val o = new Outer(2)
    assert(new o.Child(2).result == 7)
  }
}
