object Test {

  def main(args: Array[String]): Unit = {
    fun2.pacFun4(inky)
  }

  def pacFun4(unused clyde: Int) = {
    println("pacFun4")
  }

  def inky: Int = {
    println("inky")
    42
  }

  def fun2 = {
    println("fun")
    this
  }
}
