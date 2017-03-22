/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  type HKPhantom[X <: BooAny] = X

  def main(args: Array[String]): Unit = {
    fun(hkFun2(boo[Blinky]))
    fun(hkFun2(boo[Inky]))
    fun(hkFun2(boo[Pinky]))
  }

  def fun(top: BooAny): Unit = println("hk2")

  def hkFun2[Y <: BooAny](p10: HKPhantom[Y]): HKPhantom[Y] = p10
}


object Boo extends Phantom {
  type BooAny = Boo.Any
  type Blinky <: Boo.Any
  type Inky <: Blinky
  type Pinky <: Inky
  def boo[B <: Boo.Any]: B = assume[B]
}
