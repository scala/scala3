import language.experimental.captureChecking
import language.experimental.modularity
import compiletime.uninitialized

object test1:
  class File:
    def write(x: String): Unit = ???

  class Service(f: File^):
    def log = f.write("log")

  def withFile[T](op: (f: File^) => T): T =
    op(new File)

  def test =
    withFile: f =>
      val o = Service(f)
      o.log

object test2:
  class IO

  class File:
    def write(x: String): Unit = ???

  class Service(tracked val io: IO^) extends caps.Stateful:
    var file: File^{io} = uninitialized
    def log = file.write("log")

  def withFile[T](io2: IO^)(op: (f: File^{io2}) => T): T =
    op(new File)

  val io3: IO^ = IO()

  def test() =
    withFile(io3): f => // error: separation failure
      val o = Service(io3)
      o.file = f  // this is a bit dubious. It's legal since we treat class refinements
                  // as capture set variables that can be made to include refs coming from outside.
      o.log
