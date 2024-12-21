import language.experimental.captureChecking
import caps.*

trait Ref[T] { def set(x: T): T }
def test() = {

  def swap[T](x: Ref[T]^)(y: Ref[T]^{x}): Unit = ???
  def foo[T](x: Ref[T]^{cap.rd}): Unit =
    swap(x)(x)

  def bar[T](x: () => Ref[T]^{cap.rd})(y: Ref[T]^{x}): Unit =
    swap(x())(y)  // error

  def baz[T](x: Ref[T]^{cap.rd})(y: Ref[T]^{x}): Unit =
    swap(x)(y)
}

trait IO
type Op = () -> Unit
def test2(c: IO^, f: Op^{c}) = {
  def run(io: IO^)(op: Op^{io}): Unit = op()
  run(c)(f) // error: separation failure

  def bad(getIO: () => IO^, g: Op^{getIO}): Unit =
    run(getIO())(g)  // error // error: separation failure
}

def test3() = {
  def run(io: IO^)(op: Op^{io}): Unit = ???
  val myIO: IO^ = ???
  val myOp: Op^{myIO} = ???
  run(myIO)(myOp) // error: separation failure
}
