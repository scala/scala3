import dotty.unused2

object Test {

  def main(args: Array[String]): Unit = {
    {
      (x: Int @unused2) => 42
    }

    println("ok")
  }
}
