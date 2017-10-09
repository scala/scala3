import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    {
      (x: Int @unused) => 42
    }

    println("ok")
  }
}
