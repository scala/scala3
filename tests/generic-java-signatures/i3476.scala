object Test {
  def main(args: Array[String]): Unit = {
    val members = classOf[Foo].getDeclaredMethods.map(_.toGenericString)
    assert(members.forall(_ != "<java.lang.NullPointerException>"), members.mkString(", "))
    println("OK")
  }
}
class Foo {
  def m[T](x: T): Int => T = {
    def bar(y: Int): T = x
    bar _
  }
}

