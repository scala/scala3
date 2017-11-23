import MyPhantom._

object Test {
  def main(args: Array[String]): Unit = {
    val cursed = new Cursed(7)(boo)
    val cursed2 = new Cursed(7)(boo)
    foo(cursed.p)
    foo(cursed2.p)
  }

  def foo(unused x: Boo) = ()
}


class Cursed(val n: Int)(unused val p: Boo) extends AnyVal

class Cursed2[B <: Boo](val n: Int)(unused val p: B) extends AnyVal

class Cursed3[B <: Boo](val n: Int)(unused val p1: Boo, val p2: B) extends AnyVal

object MyPhantom extends Phantom {
  type Boo <: super[MyPhantom].Any
  unused def boo: Boo = assume
}
