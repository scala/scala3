import annotation.tailrec

object O6891 {
  implicit class Foo[A](val value: String) extends AnyVal {
    def bippy() = {
      @tailrec def loop(x: A): Unit = loop(x)
      ()
    }

    def boppy() = {
      @tailrec def loop(x: value.type): Unit = loop(x)
      ()
    }

    def beppy[C](c: => C) = {
      () => c
      @tailrec def loop(x: value.type): Unit = loop(x)
      () => c
      ()
    }
  }
}
