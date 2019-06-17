object Test {
  var count = 0

  lazy val x: Int = {
    if (count < 100) {
      count += 1
      ???
    }
    1
  }

  def main(args: Array[String]): Unit = {
    def fetchLazy(): Unit = try x catch { case _: Throwable => fetchLazy() }

    for (_ <- 0 until 10) {
      new Thread(() => fetchLazy()).start()
    }
  }
}
