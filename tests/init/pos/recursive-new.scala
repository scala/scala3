class A {
  val a = new A             // ok
  lazy val b = new A        // ok
  val f = () => new A       // ok
}

object Test {
  def main(args: Array[String]): Unit = new A
}
