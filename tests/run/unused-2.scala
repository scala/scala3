import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {

    def !!! : Nothing = ???
    @unused def &&& : Nothing = ???

    fun(&&&)
    try {
      fun(!!!)
    } catch {
      case e: NotImplementedError => println("OK")
    }
  }

  def fun(@unused bottom: Nothing): Unit = {
    println("fun")
  }
}
