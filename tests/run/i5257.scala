object Test {

  def main(args: Array[String]): Unit = {
    val f: Int *: Int *: EmptyTuple => Int = (x, y) => x + y
    val g: Int *: Tuple1[Int] => Int = (x, y) => x + y

    println(f((1, 2)))
    println(g((2, 3)))
  }

}
