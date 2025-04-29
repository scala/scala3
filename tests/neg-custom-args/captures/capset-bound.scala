import language.experimental.captureChecking
import caps.*

class IO

case class File(io: IO^)

def test(io1: IO^, io2: IO^) =
  def f[C^ >: {io1}](file: File^{C}) = ???
  def g[C >: {io1}](file: File^{C}) = ???
  val f1: File^{io1} = ???
  val f2: File^{io2} = ???
  val f3: File^{io1, io2} = ???
  f[{io1}](f1)
  f[{io1}](f2) // error
  f[{io1}](f3) // error
  f[{io2}](f2) // error
  f[{io1, io2}](f1)
  f[{io1, io2}](f2)
  f[{io1, io2}](f3)
  g[{io1}](f1)
  g[{io1}](f2) // error
  g[{io1}](f3) // error
  g[{io2}](f2) // error
  g[{io1, io2}](f1)
  g[{io1, io2}](f2)
  g[{io1, io2}](f3)