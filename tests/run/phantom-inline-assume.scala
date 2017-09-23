object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    any
    ()
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  inline def any: BooAny = {
    println("any")
    assume
  }
}
