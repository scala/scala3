trait ThreadedImpl {
  private val threadSync = new AnyRef
  @volatile private var wasClosed = false

  private val thread: Thread = ???
  val x: Array[Int] = ???

  final protected def isThreadRunning: Boolean =
    x(0)
    // threadSync.synchronized(!wasClosed)
    // true
    thread.isAlive && threadSync.synchronized(!wasClosed)
}
