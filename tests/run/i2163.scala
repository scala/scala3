class Base(f: Int => Int) {
  println(f(3))
}

class Child(x: Int) extends Base(y => x + y)

object Test {
  def main(args: Array[String]): Unit = new Child(4)
}
