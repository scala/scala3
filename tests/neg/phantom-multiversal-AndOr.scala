
class BooFunDef1 {
  import Universe1._
  import UniverseA._

  def fun1(unused b: One | A) = ??? // error
  def fun2(unused b: A | One) = ??? // error
  def fun3(unused b: A | One | Any) = ??? // error // error

  def fun4(unused b: A & One) = ??? // error
  def fun5(unused b: One & A) = ??? // error
  def fun6(unused b: A & One & Any) = ??? // error // error
}

object Universe1 extends Phantom {
  type One <: this.Any
}

object UniverseA extends Phantom {
  type A <: this.Any
}
