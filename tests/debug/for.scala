object Test {
  def main(args: Array[String]): Unit = {
     val b = 8 * 9  // [break] [step: f()]
     f()            // [step: val a]
     20 + b
     println(b)
  }

  def f(): Unit = {
    val a = for (i <- 1 to 5; j <- 10 to 20)        // [cont]
              yield (i, j)      // Error: incorrect reaching this line

    for (i <- 1 to 5; j <- 10 to 20)
      println(i + j)           // TODO: i is renamed to i$2 --> reduce debuggability
  }
}