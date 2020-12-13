object Test {
  def main(args: Array[String]): Unit = {
    val l = new java.util.ArrayList[String]
    l.add("foo")
    java.util.Collections.min(l)
  }
}
