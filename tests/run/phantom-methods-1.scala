/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun(phantomFun1())
  }

  def fun(top: BooAny): Unit = ()

  def phantomFun1(): Pinky = boo[Pinky]
}

object Boo extends Phantom {
  type BooAny = Boo.Any
  type Pinky <: Boo.Any
  def boo[B <: Boo.Any]: B = assume[B]
}
