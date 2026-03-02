object Test {
  def main(args: Array[String]): Unit = {
     val b = 8 * 9
     f()
     20 + b
     println(b)
  }

  def f(): Unit = {
    val a = for (i <- 1 to 5; j <- 10 to 20)
              yield (i, j)

    for (i <- 1 to 5; j <- 10 to 20)
      println(i + j)
  }
}