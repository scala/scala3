object Test {
  var count: Int = 0
  lazy val lzy: Int = {
    if (count < 10) {
      println(s"Iteration $count")
      count += 1
      lzy
    } else 42
  }

  def main(args: Array[String]): Unit = {
    println(lzy)
  }
}
