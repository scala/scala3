object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    polyfun1()
    polyfun1[Casper]()
  }

  def polyfun1[P <: Casper](): Unit = {
    println("polyfun1")
  }

}

object Boo extends Phantom {
  type Casper <: this.Any
  def boo[B <: this.Any]: B = assume
}
