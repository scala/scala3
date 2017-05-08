
class BooFunDef1 {
  import Boo._

  def fun1(b: BooAny | Any) = ??? // error
  def fun2(b: BooAny | Any | Any) = ??? // error // error
  def fun3(b: Any | BooAny | Any) = ??? // error
  def fun4(b: BooAny | BooAny | Any) = ??? // error

  def fun5(b: BooAny & Any) = ??? // error
  def fun6(b: Any & BooAny & Any) = ??? // error
  def fun7(b: BooAny & Any & Any) = ??? // error // error
  def fun8(b: Any & Any & BooAny) = ??? // error
}

object Boo extends Phantom {
  type BooAny = this.Any
}
