object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun2(Boo.nothig)
  }

  def fun2(unused bottom: BooNothing): Unit = {
    println("fun2")
  }
}

object Boo extends Phantom {
  type BooNothing = this.Nothing
  unused def nothig: BooNothing = assume
}
