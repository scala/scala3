import caps.CapSet

trait Async:
  def await[T, cap Cap](using caps.Contains[Cap, this.type])(src: Source[T, {Cap}]^): T

def foo(x: Async) = x.await(???) // error

trait Source[+T, cap Cap]:
  final def await(using ac: Async^{Cap}) = ac.await[T, {Cap}](this) // Contains[Cap, ac] is assured because {ac} <: Cap.

def test(using ac1: Async^, ac2: Async^, x: String) =
  val src1 = new Source[Int, {ac1}] {}
  ac1.await(src1) // ok
  val src2 = new Source[Int, {ac2}] {}
  ac1.await(src2) // error
