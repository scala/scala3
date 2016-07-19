/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun(
      { println("x1"); boo },
      { println("x2"); boo }
    )(
      { println("x3"); boo }
    )(
      { println("x4"); boo },
      { println("x5"); boo }
    )

    new Fun(
      { println("y1"); boo },
      { println("y2"); boo }
    )(
      { println("y3"); boo }
    )(
      { println("y4"); boo },
      { println("y5"); boo }
    )

    (new Fun2().fun)(
      { println("z1"); boo },
      { println("z2"); boo }
    )(
      { println("z3"); boo }
    )(
      { println("z4"); boo },
      { println("z5"); boo }
    )

    (new Fun2().fun2)(
      { println("w1"); boo },
      { println("w2"); boo }
    )(
      { println("w3"); boo }
    )(
      { println("w4"); boo },
      { println("w5"); boo }
    )
  }

  def fun(x1: Inky, x2: Inky)(x3: Inky)(x4: Inky, x5: Inky) = {
    println("fun")
  }

  class Fun(y1: Inky, y2: Inky)(y3: Inky)(y4: Inky, y5: Inky) {
    println("Fun")
  }

  class Fun2 {
    println("Fun2")
    def fun(z1: Inky, z2: Inky)(z3: Inky)(z4: Inky, z5: Inky) = {
      println("Fun2fun")
    }

    def fun2[T](z1: Inky, z2: Inky)(z3: Inky)(z4: Inky, z5: Inky) = {
      println("Fun2fun2")
    }
  }
}

object Boo extends Phantom {
  type Inky <: this.Any
  def boo: Inky = assume
}
