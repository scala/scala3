trait A {
  private val ? = "sigma"
  def a = ?

  val ?? = "alpha beta gamma"
  def b = ??
}

class B extends A

object Test {
  def main(args: Array[String]): Unit = {
    println(new B().a)
    println(new B().b)
  }
}
