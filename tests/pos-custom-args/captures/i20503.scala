import language.experimental.captureChecking

class List[+A]:
  def head: A = ???
  def tail: List[A] = ???
  def map[B](f: A => B): List[B] = ???
  def foreach[U](f: A => U): Unit = ???
  def nonEmpty: Boolean = ???

def runOps(ops: List[() => Unit]): Unit =
  ops.foreach(op => op())

def main(): Unit =
  val f: List[() => Unit] => Unit = (ops: List[() => Unit]) => runOps(ops)
  val _: List[() => Unit] => Unit = runOps
