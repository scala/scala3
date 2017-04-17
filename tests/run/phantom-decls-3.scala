/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {

    new Boo3(){
      type Boo1 = BooAny
    }.polyfun1(boo[Pinky])

    new Boo3(){
      type Boo1 = Blinky
    }.polyfun1(boo[Blinky])

  }

  trait Boo3 {
    println("Boo3")
    type Boo1 <: BooAny
    def polyfun1(p3: Boo1): Unit = {
      println("Boo3.polyfun1")
    }
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  type Blinky <: this.Any
  type Inky <: Blinky
  type Pinky <: Inky
  def boo[B <: BooAny]: B = assume
}
