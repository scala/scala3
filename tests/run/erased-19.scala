//> using options -language:experimental.erasedDefinitions

object Test {

  def main(args: Array[String]): Unit = {
    {
      (erased x: Int) => 42
    }

    println("ok")
  }
}
