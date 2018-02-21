object Test {

  def main(args: Array[String]): Unit = {

    unused def !!! : Nothing = ???

    try {
      fun(!!!)
      println("OK")
    } catch {
      case e: NotImplementedError =>
    }
  }

  def fun(unused bottom: Nothing): Unit = {
    println("fun")
  }
}
