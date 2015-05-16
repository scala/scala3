trait Hello {
  println("Hello")
  val x: Int = 1
  println("World")
}

class A extends Hello

object Test {
  def main(args: Array[String]): Unit = {
    new A
  }
}
