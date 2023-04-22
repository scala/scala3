import language.experimental.captureChecking
import annotation.capability
import compiletime.uninitialized

object test1:
  class File:
    def write(x: String): Unit = ???

  class Service(f: {*} File):
    def log = f.write("log")

  def withFile[T](op: (f: {*} File) => T): T =
    op(new File)

  def test =
    withFile: f =>
      val o = Service(f)
      o.log

object test2:
  @capability class IO

  class File:
    def write(x: String): Unit = ???

  class Service(io: IO):
    var file: {io} File = uninitialized
    def log = file.write("log")

  def withFile[T](io: IO)(op: (f: {io} File) => T): T =
    op(new File)

  def test(io: IO) =
    withFile(io): f =>
      val o = Service(io)
      o.file = f
      o.log
