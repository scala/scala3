
object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    Boo.assume1
    Boo.assume2
    Boo.assume3
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  def assume1: BooAny = assume
  def assume2: BooAny = this.assume
  def assume3: BooAny = Boo.assume
}
