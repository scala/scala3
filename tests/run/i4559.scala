trait A {
  lazy val x = { println("super[A].x()"); 123 }
}

class B extends A {
  override lazy val x = 456
}

object Test { def main(args: Array[String]): Unit = { new B() } }
