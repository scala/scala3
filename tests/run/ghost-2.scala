object Test {

  def main(args: Array[String]): Unit = {

    ghost def !!! : Nothing = ???

    try {
      fun(!!!)
      println("OK")
    } catch {
      case e: NotImplementedError =>
    }
  }

  def fun(ghost bottom: Nothing): Unit = {
    println("fun")
  }
}
