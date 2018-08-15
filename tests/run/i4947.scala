object Test {

  transparent def track[T](f: => T): T = f

  def main(args: Array[String]): Unit = {
    try {
      track {
        val a = 9
        throw new Exception("failed")
      }
    } catch {
      case ex: Throwable =>
        println(ex.getMessage)
        println(ex.getStackTrace.head)
    }
  }

}
