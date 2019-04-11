object Test {

  def main(args: Array[String]): Unit = {
    fun2.pacFun4(inky)
  }

  def pacFun4 erased (clyde: Int) = {
    println("pacFun4")
  }

  erased def inky: Int = {
    println("inky") // in erased function
    42
  }

  def fun2 = {
    println("fun")
    this
  }
}
