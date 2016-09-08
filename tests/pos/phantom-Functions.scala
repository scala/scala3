
import Phantoms._

class PhantomFunTArgs1 extends Function0[Casper] {
  def apply() = casper
}

class PhantomFunTArgs2 extends Function1[Casper, Casper] {
  def apply(p1: Casper) = casper
}

object Phantoms extends Phantom {
  type Casper <: this.Any
  def casper: Casper = assume[Casper]
}
