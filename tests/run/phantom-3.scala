/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun3(boo[Blinky], boo[Pinky])
    fun3(boo[Inky], boo[Pinky])
    fun3(boo[Pinky], boo[Casper])
  }

  def fun3(x1: Blinky, x2: Inky): Unit = {
    println("fun3")
  }
}

object Boo extends Phantom {
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  type Casper = Pinky
  def boo[B <: Blinky]: B = assume[B]
}
