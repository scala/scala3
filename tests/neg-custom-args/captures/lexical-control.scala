import language.experimental.captureChecking
import caps.*

trait Label extends SharedCapability:
  type Fv^ // the capability set occurring freely in the `block` passed to `boundary` below.

def boundary[T, C^](block: Label{type Fv = {C} } ->{C} T): T = ??? // link label and block capture set
def suspend[U](label: Label)[D^ <: {label.Fv}](handler: () ->{D} U): U = ??? // note the path

def test =
  val x = 1
  boundary: outer =>
    val y = 2
    boundary: inner =>
      val z = 3
      val w = suspend(outer) {() => z} // ok
      val v = suspend(inner) {() => y} // ok
      val u = suspend(inner): () =>
        suspend(outer) {() => y} // ok
        suspend(outer) {() => y} // ok
        y
      suspend(outer) { () => // error
        suspend(outer) {() => y }
      }
      suspend(outer): () => // error  (leaks the inner label)
        println(inner)
        x + y + z