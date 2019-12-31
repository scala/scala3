package dotty.tools.dotc.util
class Scheduler extends Thread with

  private var execTime: Long = Long.MaxValue
  private var task: () => Unit = _
  private var stopped: Boolean = false

  def scheduleAfter(after: Long)(task: => Unit) = synchronized {
    this.task = () => task
    this.execTime = System.currentTimeMillis + after
  }

  def cancel(): Unit = synchronized {
    execTime = Long.MaxValue
  }

  def shutdown(): Unit = synchronized {
    stopped = true
  }

  def poll(): Unit = synchronized {
    if System.currentTimeMillis() > execTime then
      task()
      execTime = Long.MaxValue
  }

  override def run(): Unit =
    while !stopped do
      poll()
      Thread.sleep(1)

  start()
end Scheduler