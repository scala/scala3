// scalajs: --skip

class C {
  inline def one = 1
  def foo = {
    def notfive(i: Int) = println(i)
    inline def five = one + 4
    Seq(5).map { i =>
      i + five
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    classOf[C].getDeclaredMethods.map(_.toString).sorted.foreach(println)
  }
}
