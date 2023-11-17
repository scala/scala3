import language.experimental.captureChecking
trait IO
def test1(): Unit =
  val id: IO^ -> IO^ = x => x
  val id1: IO^ -> IO^{id*} = id  // error

def test2(): Unit =
  val f: (IO^ => Unit) => Unit = ???
  val f1: (IO^{f*} => Unit) ->{f*} Unit = f  // ok

def test3(): Unit =
  val f: IO^ -> (IO^ => Unit) => Unit = ???
  val f1: IO^ -> (IO^{f*} => Unit) => Unit = f  // error
  val f2: IO^ -> (IO^ => Unit) ->{f*} Unit = f  // error
