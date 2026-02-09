import language.experimental.captureChecking
trait Collection[T]
trait IO
def empty[T]: Collection[T]^ = ???
def emptyAlt[T](): Collection[T]^ = ???
def newIO: IO^ = ???
def test1(): Unit =
  val t1: Collection[Int]^ = empty[Int]  // ok
  val t2: IO^ = newIO  // ok
  val t3: Collection[Int]^ = emptyAlt[Int]() // ok
