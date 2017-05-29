
class phantomTypeParamBounds1 {
  def fun5[X >: Boo.Nothing <: Any] = ??? // error
  def fun6[X >: Nothing <: Boo.Any] = ??? // error
}

object Boo extends Phantom {
  def boo[B <: this.Any]: B = assume
}
