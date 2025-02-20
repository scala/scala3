object Test:
  def main(args: Array[String]): Unit =
    try throwException()
    catch case e: Exception => ()

  def throwException(): Unit =
    throw new Exception("foo")
