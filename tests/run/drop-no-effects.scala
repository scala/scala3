object Test {
  def main(args: Array[String]): Unit = {
    var run = false

    val one = {
      run = true
      1
    }.asInstanceOf[Int]

    assert(run)
  }
}
