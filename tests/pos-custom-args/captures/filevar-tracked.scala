import language.experimental.captureChecking
import language.experimental.modularity
import annotation.capability
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
  class IO extends caps.Capability

  class File:
    def write(x: String): Unit = ???

  class Service(tracked val io: IO):
    var file: File^{io} = uninitialized
    def log = file.write("log")

  def withFile[T](io2: IO)(op: (f: File^{io2}) => T): T =
    op(new File)

  def test(io3: IO) =
    withFile(io3): f =>
      val o = Service(io3)
      o.file = f
      o.log
