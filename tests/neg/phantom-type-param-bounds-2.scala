
class phantomTypeParamBounds2 {
  def fun1[X <: Boo.Any & Any] = ??? // error
  def fun2[X <: Boo.Any | Any] = ??? // error
  def fun3[X >: Boo.Nothing & Nothing] = ??? // error
  def fun4[X >: Boo.Nothing | Nothing] = ??? // error

  def fun5[X >: Boo.Any & Any <: Boo.Any & Any] = ??? // error // error
}

object Boo extends Phantom {
  def boo[B <: Boo.Any]: B = assume
}
