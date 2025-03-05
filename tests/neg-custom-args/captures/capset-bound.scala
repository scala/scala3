import caps.*

class IO

case class File(io: IO^)

def test(io1: IO^, io2: IO^) =
  def f[C >: CapSet^{io1} <: CapSet^](file: File^{C}) = ???
  val f1: File^{io1} = ???
  val f2: File^{io2} = ???
  val f3: File^{io1, io2} = ???
  f[CapSet^{io1}](f1)
  f[CapSet^{io1}](f2) // error
  f[CapSet^{io1}](f3) // error
  f[CapSet^{io2}](f2) // error
  f[CapSet^{io1, io2}](f1)
  f[CapSet^{io1, io2}](f2)
  f[CapSet^{io1, io2}](f3)