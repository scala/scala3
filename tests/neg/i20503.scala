import language.experimental.captureChecking

class List[+A]:
  def head: A = ???
  def tail: List[A] = ???
  def map[B](f: A => B): List[B] = ???
  def foreach[U](f: A => U): Unit = ???
  def nonEmpty: Boolean = ???

def runOps(ops: List[() => Unit]): Unit =
  // See i20156, due to limitation in expressiveness of current system,
  // we could map over the list of impure elements. OK with existentials.
  ops.foreach(op => op())

def main(): Unit =
  val f: List[() => Unit] -> Unit = runOps  // error
