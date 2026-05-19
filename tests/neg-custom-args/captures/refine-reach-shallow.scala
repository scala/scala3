import language.experimental.captureChecking
import caps.fresh
trait IO
def test1(): Unit =
  val f: IO^ => IO^ = x => x // error
  val g: IO^ => IO^{f*} = f  // now OK
def test2(): Unit =
  val f: [R] -> (IO^ => R) -> R = ???
  val ff = f
  val g: [R] -> (IO^{f*} => R) -> R = f  // error // error
def test3(): Unit =
  val f: [R] -> (IO^ -> R) -> R = ???
  val g: [R] -> (IO^{f*} -> R) -> R = f  // error // error
def test4(): Unit =
  val xs: List[IO^] = ???
  val ys: List[IO^{xs*}] = xs  // ok
def test5(): Unit =
  val f: [R] -> (IO^ -> R) -> IO^{fresh} = ???
  val g: [R] -> (IO^ -> R) -> IO^{f*} = f  // error
  val h: [R] -> (IO^{f*} -> R) -> IO^ = f  // error
