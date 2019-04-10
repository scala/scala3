object Test {

  def main(args: Array[String]): Unit = {

    erased def !!! : Nothing = ???

    try {
      fun(!!!)
      println("OK")
    } catch {
      case e: NotImplementedError =>
    }
  }

  def fun erased (bottom: Nothing): Unit = {
    println("fun")
  }
}
