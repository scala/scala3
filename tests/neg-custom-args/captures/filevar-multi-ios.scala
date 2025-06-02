import language.experimental.modularity
import compiletime.uninitialized

class IO extends caps.Capability

class File:
  def write(x: String): Unit = ???

object test1:

  class Service(val io: IO, val io2: IO):
    var file: File^{io} = uninitialized
    var file2: File^{io2} = uninitialized
    def log = file.write("log")

  def withFile[T](io: IO)(op: File^{io} => T): T =
    op(new File)

  def test(io3: IO, io4: IO) =
    withFile(io3): f => // error
      val o = Service(io3, io4)
      o.file = f
      o.file2 = f
      o.log

object test2:

  class Service(tracked val io: IO, tracked val io2: IO):
    var file: File^{io} = uninitialized
    var file2: File^{io2} = uninitialized
    def log = file.write("log")

  def withFile[T](io: IO)(op: File^{io} => T): T =
    op(new File)

  def test(io3: IO, io4: IO) =
    withFile(io3): f => // error
      val o = Service(io3, io4)
      o.file = f
      o.file2 = f
      o.log
