abstract class A {
  val s: Int
  assert(s == 1)
}

class B(val s: Int) extends A

object Test extends B(1) {
  def main(args: Array[String]): Unit = {
    s
  }
}
