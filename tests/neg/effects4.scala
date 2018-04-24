
import Effect.isPure
object Test {

  def f(implicit erased x: CanThrow[NullPointerException]) = ()

  var cond = true

  def capture(implicit erased x: CanThrow[NullPointerException]) = {
    if (cond)
      // TODO wrong error message
      { (y: Int) => f }  // error: Type argument CanThrow[NullPointerException](x) does not conform to upper bound Any
    else {
      class Capture {
        def bar = f      // error:
      }
    }
  }
  def capture2() = {
    implicit erased val hidden: CanThrow[NullPointerException] = Effect.canThrowNPE // error: Type argument CanThrow[NullPointerException](canThrowNPE) does not conform to upper bound Any

    if (cond)
      { (y: Int) => f }  // error:
    else {
      class Capture {
        def bar = f      // error:
      }
    }
  }
}

trait PureFunction[-A, +B] extends Function[A, B] {
  type Eff = Pure
  def apply(x: A)(implicit eff: Eff): B
}