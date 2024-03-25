import scala.annotation.publicInBinary

inline trait A(@publicInBinary private[A] val a: Int): // TODO remove `val`?
  def f: Int = a
  def g(b: Int): Int = a + b
end A

class B extends A(4)
