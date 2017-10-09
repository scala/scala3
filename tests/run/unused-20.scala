import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    fun { (x: Int @unused) =>
      println("lambda")
      42
    }

  }

  def fun(f: (Int @unused) => Int): Int = {
    f(35)
  }
}
