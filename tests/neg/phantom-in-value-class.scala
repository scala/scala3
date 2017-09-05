import MyPhantom._


class Cursed1(val p: Boo) extends AnyVal // error

class Cursed2(val n: Int)(val a: Int) extends AnyVal // error

object MyPhantom extends Phantom {
  type Boo <: super[MyPhantom].Any
  def boo: Boo = assume
}
