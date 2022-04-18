object FakePrintWriter extends java.io.PrintWriter("fake-print-writer") {
  @volatile var messages = List.empty[String]
  def resetMessages = messages = List.empty[String]
  override def println(x: String): Unit = messages = x :: messages
  override def print(x: String): Unit = messages = x :: messages
}
