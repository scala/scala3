
import Effect.{canThrowNPE => _}
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
}
