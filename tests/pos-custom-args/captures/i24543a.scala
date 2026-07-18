import scala.language.experimental.captureChecking
class Ref
case class Box[+T](elem: T)

def test1(a: Ref^): Unit =
  def hh[T <: Ref^{a}](x: Box[T]): Unit =
    val y: (Ref^{a}, Int) = (x.elem, 1)
    val z = (x.elem, 1)
    val _: (Ref^{a}, Int) = z
