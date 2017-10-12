import dotty.unused2

object Test {

  def main(args: Array[String]): Unit = {
    fun { (x: Int @unused2) =>
      println("lambda")
      42
    }

  }

  def fun(f: (Int @unused2) => Int): Int = {
    f(35)
  }
}
