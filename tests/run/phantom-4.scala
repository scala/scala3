/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._
  
  def main(args: Array[String]): Unit = {
    fun4(3, 4, boo[Blinky], boo[Pinky])
    fun4(5, 6, boo[Inky], boo[Pinky])
    fun4(7, 8, boo[Pinky], boo[Casper])
  }

  def fun4(n: Int, n2: Int, top: Blinky, bottom: Pinky): Unit = {
    println("fun4")
  }

}

object Boo extends Phantom {
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  type Casper = Pinky
  def boo[B <: Blinky]: B = assume
}
