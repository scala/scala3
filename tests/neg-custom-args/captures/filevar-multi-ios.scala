import language.experimental.modularity
import compiletime.uninitialized

class IO extends caps.SharedCapability

class File:
  def write(x: String): Unit = ???

object test1:

  class Service(val io: IO, val io2: IO) extends caps.Stateful:
    var file: File^{io} = uninitialized
    var file2: File^{io2} = uninitialized
    def log = file.write("log")

  def withFile[T](io: IO)(op: File^{io} => T): T =
    op(new File)

  def test(io3: IO, io4: IO) =
    withFile(io3): f =>
      val o = Service(io3, io4)
      o.file = f  // error
      o.file2 = f // error
      o.log

object test2:

  class Service(tracked val io: IO, tracked val io2: IO) extends caps.Stateful:
    var file: File^{io} = uninitialized
    var file2: File^{io2} = uninitialized
    def log = file.write("log")

  def withFile[T](io: IO)(op: File^{io} => T): T =
    op(new File)

  def test(io3: IO, io4: IO) =
    withFile(io3): f =>
      val o = Service(io3, io4)
      o.file = f
      o.file2 = f // error
      o.log
