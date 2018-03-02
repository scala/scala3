object Test {

  def main(args: Array[String]): Unit = {
    val f: ghost Int => Int =
      ghost (x: Int) => {
        x // error
      }

    val f2: ghost Int => Int =
      ghost (x: Int) => {
        foo(x)
      }

    def foo (ghost i: Int) = 0
  }

}
