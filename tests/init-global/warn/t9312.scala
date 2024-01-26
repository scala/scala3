object DeadLockTest {
  def main(args: Array[String]): Unit = {
    def run(block: => Unit): Unit =
      new Thread(new Runnable {def run(): Unit = block}).start()

    run {println(Parent.Child1)}
    run {println(Parent.Child2)}

  }

  object Parent {
    trait Child {
      Thread.sleep(2000) // ensure concurrent behavior
      val parent = Parent
      def siblings = parent.children - this
    }

    object Child1 extends Child
    object Child2 extends Child

    final val children = Set(Child1, Child2)
  }
}

