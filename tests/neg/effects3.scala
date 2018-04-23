
import Effect.isPure
object Test {

  def f(implicit x: CanThrow[NullPointerException]) = ()

  var cond = true

  def capture(implicit x: CanThrow[NullPointerException]) = {
    if (cond)
      { (y: Int) => f }  // error: illegal capture
    else {
      class Capture {
        def bar = f      // error: illegal capture
      }
    }
  }
  def capture2() = {
    implicit val hidden: CanThrow[NullPointerException] = Effect.canThrowNPE
    if (cond)
      { (y: Int) => f }  // error: illegal capture
    else {
      class Capture {
        def bar = f      // error: illegal capture
      }
    }
  }
}

trait PureFunction[-A, +B] extends Function[A, B] {
  type Eff = Pure
  def apply(x: A)(implicit eff: Eff): B
}