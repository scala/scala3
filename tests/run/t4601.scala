class A

trait B {
  self: A =>

  def test: Unit = {
    println(Symbol("blubber"))
  }
}

object Test extends A with B {
  def main(args: Array[String]): Unit = {
    test
  }
}
