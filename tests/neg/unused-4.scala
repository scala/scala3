object Test {

  def main(args: Array[String]): Unit = {
    val f: unused Int => Int =
      unused (x: Int) => {
        x // error
      }

    val f2: unused Int => Int =
      unused (x: Int) => {
        foo(x)
      }

    def foo (unused i: Int) = 0
  }

}
