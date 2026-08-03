trait T {
  def m0(): String = synchronized { "trait" }
}

class C extends T {
  def m1(): String = synchronized { "class" }
}

object C {
  def m2(): String = synchronized { "static" }

  @scala.annotation.static
  def m3(): String = synchronized { "object static" }
}

object Test {
  def main(args: Array[String]): Unit =
    val c = new C()
    println(c.m0())
    println(c.m1())
    println(C.m2())
    println(C.m3())
}
