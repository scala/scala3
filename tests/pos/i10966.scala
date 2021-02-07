object Main {
  def main(args: Array[String]): Unit = {
    def apply(a: Int, b: Int): Unit = {}
    apply(1, 2)

    // The following will work:
    def f(a: Int, b: Int): Unit = {}
    f(1, 2)
  }

  def foo() = {
    // The following will not work either:
    def `apply`(a: Int): Unit = {}
    `apply`(1)
  }
}
