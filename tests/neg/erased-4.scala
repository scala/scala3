object Test {

  def main(args: Array[String]): Unit = {
    def foo  erased (i: Int) = 0

    val f: erased Int => Int =
      erased (x: Int) => {
        x // error
      }

    val f2: erased Int => Int =
      erased (x: Int) => {
        foo(x)
      }
  }

}
