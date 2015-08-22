case class A(a: String*){}

object Test {
  def main(args: Array[String]): Unit = {
    val s = new A("a", "b");
    s.a
  }
}
