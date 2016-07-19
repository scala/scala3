/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._
  
  def main(args: Array[String]): Unit = {
    fun5(boo[Blinky])(15)(boo[Pinky])(16)
    fun5(boo[Inky])(17)(boo[Pinky])(18)
    fun5(boo[Pinky])(19)(boo[Casper])(20)
  }

  def fun5(top: Blinky)(n: Int)(bottom: Clyde)(n2: Int): Unit = {
    println("fun5")
  }
}

object Boo extends Phantom {
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  type Clyde >: Pinky <: Inky
  type Casper = Pinky
  def boo[B <: Blinky]: B = assume[B]
}
