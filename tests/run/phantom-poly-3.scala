/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    polyfun3(boo[Blinky])
    polyfun3(boo[Inky])
    polyfun3(boo[Pinky])
  }

  def polyfun3[G <: BooAny, I <: G](q: I): Unit = {
    println("polyfun3")
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  def boo[B <: Blinky]: B = assume
}
