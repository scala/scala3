//> using options -Yimports:scala,scala.Predef
// Tests quality of error messages
class IO extends caps.SharedCapability

def test(io: IO): Unit =
  var v: IO = io
  var w: () => Unit = () => ()
  def f1(x: IO) =
    def g() = println(x)
    v = x  // error
    w = g  // error

  def withFile[T](op: IO => T): T =
    val io = IO()
    op(io)

  def id(x: IO): x.type = x
  def id2(x: IO): IO = x

  withFile(io => io)         // error
  withFile(id)               // error
  withFile(x => id(x))       // error
  withFile(id2)              // error, note mentions any since we never have a more specific include failure
  withFile(x => id2(x))      // error, note mentions any since we never have a more specific include failure
  withFile(identity)         // error, note mentions any since we never have a more specific include failure
  withFile(x => identity(x)) // error, note mentions any since we never have a more specific include failure

  withFile: io =>     // error
    () => println(io)

  val f2: IO => IO = (x: IO) => x  // error
  val f3 = (x: IO) => x
  val f4: IO => IO = f3  // error

