/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun1(
      { println("x1"); boo },
      { println("x2"); boo }
    )(
      { println("x3"); 0 }
    )(
      { println("x4"); boo },
      { println("x5"); boo }
    )

    fun2(
      { println("y1"); 1 },
      { println("y2"); 2 }
    )(
      { println("y3"); boo }
    )(
      { println("y4"); boo },
      { println("y5"); boo }
    )

    fun3(
      { println("z1"); boo },
      { println("z2"); boo }
    )(
      { println("z3"); 4 }
    )(
      { println("z4"); 5 },
      { println("z5"); 6 }
    )
  }

  def fun1[T](x1: Inky, x2: Inky)(x3: T)(x4: Inky, x5: Inky) = {
    println("fun1")
  }

  def fun2[T](x1: T, x2: T)(x3: Inky)(x4: Inky, x5: Inky) = {
    println("fun2")
  }

  def fun3[T](x1: Inky, x2: Inky)(x3: T)(x4: T, x5: T) = {
    println("fun3")
  }

}

object Boo extends Phantom {
  type Inky <: this.Any
  def boo: Inky = assume
}
