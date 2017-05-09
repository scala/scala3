
class PhantomFun1 extends Boo.Function2[Boo.Casper, Int, Unit] { // error: Type argument Int does not conform to upper bound Boo.Any
  def apply(x1: Boo.Casper, x2: Int): Unit = ()
}

class PhantomFun2 extends Boo.Function2[Int, Boo.Casper, Unit] { // error: Type argument Int does not conform to upper bound Boo.Any
  def apply(x1: Boo.Casper, x2: Int): Unit = ()
}

class Fun extends Function2[Int, Int, Unit] {
  def apply(x1: Int, x2: Int): Unit = ()
}

object Boo extends Phantom {
  type Casper <: this.Any
}
