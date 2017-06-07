object Test {
  var count: Int = 0
  def getLzy = {
   val read = lzy
   println("Lzy has been read as: " +read)
   read
 }

  lazy val lzy: Int = {
    if (count < 10) {
      println(s"Iteration $count")
      count += 1
      getLzy + 1
    } else 42
  }

  def main(args: Array[String]): Unit = {
    println(lzy)
    getLzy
  }
}
