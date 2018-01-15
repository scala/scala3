class C {
  def m1_:(f: Int) = ()
  def m2_:(f: => Int) = ()
}

object Test {
  def foo() = { println("foo") ; 5 }

  def main(args: Array[String]): Unit = {
    val c = new C
    foo() m1_: c
    foo() m2_: c
  }
}
