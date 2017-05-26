/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    new Boo2().polyfun1(boo[Blinky])
    new Boo2().polyfun1(boo[Inky])
    new Boo2().polyfun1(boo[Pinky])
  }

  type Boo3 = BooAny

  class Boo2 {
    println("Boo2")
    def polyfun1(p2: Boo3): Unit = {
      println("Boo2.polyfun1")
    }
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  type Casper = Pinky
  def boo[B <: this.Any]: B = assume
}
