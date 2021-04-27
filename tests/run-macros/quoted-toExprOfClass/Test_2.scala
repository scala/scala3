object Test {
  def main(args: Array[String]): Unit = {
    val a: Map[String, Class[?]] = wildcard
    val b: Map[String, Class[Int]] = noWildcard
    println(a("foo"))
    println(a("bar"))
    println(b("foo"))
    println(b("bar"))
  }
}
