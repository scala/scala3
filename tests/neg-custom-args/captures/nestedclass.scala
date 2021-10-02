class CC
type Cap = {*} CC

abstract class C:
  def head: String

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  def g(x: String): String = if cap2 == cap2 then "" else "a"

  val xs =
    class Cimpl extends C:
      def head = f("")
    new Cimpl
  val xsc: C = xs  // error
