import language.experimental.captureChecking
// no separation checking
import caps.*
class IO
class Ref[X](init: X):
  private var _data = init
  def get: X = _data
  def put(y: X): Unit = _data = y
def runIO(op: IO^ => Unit): Unit = ()
def test1(a: IO^, b: IO^, c: IO^): Unit =
  val r: Ref[IO^] = Ref(a)
  r.put(b) // ok
  def outer(x: IO^): Unit =
    r.put(x)  // error
  r.put(c) // ok
  runIO: (innerIO: IO^) =>
    r.put(innerIO)  // error
  runIO: innerIO =>
    r.put(innerIO) // error
