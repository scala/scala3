object Test {

  def main(args: Array[String]): Unit = {
    unusedFun0
    unusedFun1()
    unusedFun2(foo)
  }

  def foo = {
    println("foo")
    42
  }

  unused def unusedFun0: Int = {
    println("unusedFun0")
    42
  }

  unused def unusedFun1(): Int = {
    println("unusedFun1")
    42
  }

  unused def unusedFun2(a: Int): Int = {
    println("unusedFun2")
    42
  }
}
