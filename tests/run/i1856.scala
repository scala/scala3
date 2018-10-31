object Test {
  var count = 0
  lazy val lzy: Int = {
    if (count < 10) {
      println(s"Iteration $count")
      count += 1
      lzy
    } else 42
  }

  def lzy2 = {
    var countLocal = 0
    lazy val lzyLocal: Int = {
      if (countLocal < 10) {
        println(s"Iteration $countLocal")
        countLocal += 1
        lzyLocal
      } else 42
    }
    lzyLocal
  }

  def main(args: Array[String]): Unit = {
    println(lzy)
    println(lzy2)
  }
}
