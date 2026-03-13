class ChildScala extends GenericBaseIntermidiateJava[Int] {
  // override def someMethod() = ???
}
object Test {
  def main(args: Array[String]): Unit = {
    val c = classOf[ChildScala]
    assert(
      c.getGenericInterfaces.length == c.getInterfaces.length,
      s"mismatch between ${c.getGenericInterfaces.mkString("Array(", ", ", ")")} and ${c.getInterfaces.mkString("Array(", ", ", ")")}"
    )
  }
}