import language.experimental.captureChecking
import caps.*

trait A extends SharedCapability

object R:
  given a: A = new A {}

def f(x: Any)(using A): Unit = println(x)
def test =
  import R.given
  f("test") // ok
  val xs: List[Int] = List(1, 2, 3)
  xs.foreach((x: Any) => f(x)(using R.a))
  xs.foreach(f)
  // error: Found: (x: Any) ->{R} Unit, Required: Int ->{any} Unit
  // same error for map: xs.map(f)