//> using options -explain

abstract class A:
  type Props
  def apply(p: Props) = ()

type UndefOr2[A] = A | Unit
object D extends A: // error
  case class Props()
  def apply(a: UndefOr2[String]) = ()
