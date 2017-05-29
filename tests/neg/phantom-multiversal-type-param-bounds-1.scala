
class phantomTypeParamBounds1 {
  import Universe1._
  import UniverseA._

  def fun1[X >: OneNothing <: AAny] = ??? // error
  def fun2[X >: ANothing <: OneAny] = ??? // error
}

object Universe1 extends Phantom {
  type OneAny = this.Any
  type OneNothing = this.Nothing
}

object UniverseA extends Phantom {
  type AAny = this.Any
  type ANothing = this.Nothing
}
