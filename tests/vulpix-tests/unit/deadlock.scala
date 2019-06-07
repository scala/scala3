object Test {
  class Lock
  val lock1 = new Lock
  val lock2 = new Lock

  private[this] var took1: Boolean = false
  private[this] var took2: Boolean = false

  val thread1 = new Thread {
    override def run(): Unit = {
      lock1.synchronized {
        took1 = true
        while (!took2) Thread.sleep(100)
        lock2.synchronized {
          println("thread1 in lock2!")
        }
      }
      println("thread1, done!")
    }
  }

  val thread2 = new Thread {
    override def run(): Unit = {
      lock2.synchronized {
        took2 = true
        while (!took1) Thread.sleep(100)
        lock1.synchronized {
          println("thread2 in lock1!")
        }
      }
      println("thread2, done!")
    }
  }

  def main(args: Array[String]): Unit = {
    thread1.start() // takes lock1 then sleeps 1s - tries to take lock2
    thread2.start() // takes lock2 then sleeps 1s - tries to take lock1

    thread1.join() // wait for threads to complete, can't because deadlock!
    thread2.join()
  }
}
