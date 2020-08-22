abstract class A {
  @throws(classOf[Throwable])
  def onReceive(message: Any): Unit
}
