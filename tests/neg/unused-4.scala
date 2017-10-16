object Test {

  def main(args: Array[String]): Unit = {
    val f: unused Int => Int =
      unused (x: Int) => {
        x // error
      }

    unused val f2: unused Int => Int =
      unused (x: Int) => {
        x
      }

  }

}
