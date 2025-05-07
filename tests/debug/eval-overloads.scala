object Test:
  def main(args: Array[String]): Unit =
    println("Hello, World!")

  trait A
  class B extends A

  private def m(): String = "m"
  private def m(n: Int): String = s"m($n: Int)"
  private def m(b: Boolean): String = s"m($b: Boolean)"
  private def m(str: String): String = s"m($str: String)"
  private def m(a: A): String = s"m(a: A)"
  private def m(b: B): String = s"m(b: B)"
  private def m(xs: Array[Int]): String = s"m(xs: Array[Int])"
  private def m(xs: Array[A]): String = s"m(xs: Array[A])"
  private def m(xs: Array[Array[Int]]): String = s"m(xs: Array[Array[Int]])"
  private def m1(xs: Seq[Int]): String = xs.toString
  private def m1(xs: Seq[Boolean]): Int = xs.count(identity)
