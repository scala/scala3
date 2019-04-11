object Test {

  def main(args: Array[String]): Unit = {
    fun({ println("x1"); boo })({ println("x2"); boo })

    new Fun({ println("y1"); boo })({ println("y2"); boo })

    (new Fun2().fun)({ println("z1"); boo })({ println("z2"); boo })
  }

  def fun erased (x1: Int) erased (x2: Int) = {
    println("fun")
  }

  class Fun erased (y1: Int) erased (y2: Int) {
    println("Fun")
  }

  class Fun2 {
    println("Fun2")
    def fun erased (z1: Int) erased (z2: Int) = {
      println("Fun2fun")
    }
  }

  def boo: Int = {
    println("boo")
    42
  }

}
