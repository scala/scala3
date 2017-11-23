
object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    foo(Boo.assume1)
    foo(Boo.assume2)
    foo(Boo.assume3)
  }

  def foo(unused x: Boo.BooAny) = ()
}

object Boo extends Phantom {
  type BooAny = this.Any
  unused def assume1: BooAny = assume
  unused def assume2: BooAny = this.assume
  unused def assume3: BooAny = Boo.assume
}
