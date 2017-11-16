object Test {

  def main(args: Array[String]): Unit = {

    unused def !!! : Null = ???

    try {
      fun(!!!)
      println("OK")
    } catch {
      case e: NotImplementedError =>
    }
  }

  def fun(unused bottom: Null): Unit = {
    println("fun")
  }
}
