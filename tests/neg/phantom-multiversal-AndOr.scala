
class BooFunDef1 {
  import Universe1._
  import UniverseA._

  def fun1(b: One | A) = ??? // error
  def fun2(b: A | One) = ??? // error
  def fun3(b: A | One | Any) = ??? // error // error

  def fun4(b: A & One) = ??? // error
  def fun5(b: One & A) = ??? // error
  def fun6(b: A & One & Any) = ??? // error // error
}

object Universe1 extends Phantom {
  type One <: this.Any
}

object UniverseA extends Phantom {
  type A <: this.Any
}
