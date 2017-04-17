
/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    foo
    foo
    foo = {
      println("foo2")
      any
    }
    foo

  }

  var foo = {
    println("foo")
    any
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  def any: BooAny = assume
}
