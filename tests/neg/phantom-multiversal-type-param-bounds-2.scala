
class phantomTypeParamBounds2 {
  import Universe1._
  import UniverseA._

  def fun1[X <: One & A] = ??? // error
  def fun2[X <: One | A] = ??? // error
  def fun3[X >: OneNothing & ANothing] = ??? // error
  def fun4[X >: OneNothing | ANothing] = ??? // error

  def fun5[X >: One & A <: One & A] = ??? // error // error
}

object Universe1 extends Phantom {
  type One <: this.Any
  type OneNothing = this.Nothing
}

object UniverseA extends Phantom {
  type A <: this.Any
  type ANothing = this.Nothing
}
