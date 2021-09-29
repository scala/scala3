// No longer valid
class C
type Cap = C @retains(*)
type Top = Any @retains(*)

type T = (x: Cap) => List[String @retains(x)] => Unit // error
val x: (x: Cap) => Array[String @retains(x)] = ??? // error
val y = x

def test: Unit =
  def f(x: Cap) = // ok
    val g = (xs: List[String @retains(x)]) => ()
    g
  def f2(x: Cap)(xs: List[String @retains(x)]) = ()
  val x = f // error
  val x2 = f2 // error
  val y = f(C()) // ok
  val y2 = f2(C()) // ok
  ()
