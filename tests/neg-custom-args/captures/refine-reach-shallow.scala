import language.experimental.captureChecking
trait IO
def test1(): Unit =
  val f: IO^ => IO^ = x => x
  val g: IO^ => IO^{f*} = f  // error
def test2(): Unit =
  val f: [R] -> (IO^ => R) -> R = ???
  val g: [R] -> (IO^{f*} => R) -> R = f  // error
def test3(): Unit =
  val f: [R] -> (IO^ -> R) -> R = ???
  val g: [R] -> (IO^{f*} -> R) -> R = f  // error
def test4(): Unit =
  val xs: List[IO^] = ???
  val ys: List[IO^{xs*}] = xs  // ok
def test5(): Unit =
  val f: [R] -> (IO^ -> R) -> IO^ = ???
  val g: [R] -> (IO^ -> R) -> IO^{f*} = f  // ok
  val h: [R] -> (IO^{f*} -> R) -> IO^ = f  // error
