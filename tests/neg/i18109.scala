package foo {}

package bar {
  object Test {
    def qux[A] = 123
    def main(args: Array[String]): Unit = {
      val y = qux[foo.type] // error
      val x = valueOf[foo.type] // error
    }
  }
}