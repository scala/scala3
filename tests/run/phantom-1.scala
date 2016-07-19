
/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun1(Boo.any)
  }

  def fun1(boo: BooAny): Unit = {
    println("fun1")
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  def any: BooAny = assume[BooAny]
}
