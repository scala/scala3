object Test {
  def main(args: Array[String]) = {
    val x: Int => Unit =
      y => println(x) // error: `x` is a forward reference
    implicit val z: String => Unit =
      y => println(implicitly[String => Unit]) // error: `z` is a forward reference
  }
}

