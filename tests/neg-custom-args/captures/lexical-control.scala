import language.experimental.captureChecking
import caps.*

trait Label extends Capability:
  cap type Fv // the capability set occurring freely in the `block` passed to `boundary` below.

def boundary[T, cap C](block: Label{cap type Fv = {C} } ->{C} T): T = ??? // link label and block capture set
def suspend[U](label: Label)(handler: () ->{label.Fv} U): U = ??? // note the path

def test =
  val x = 1
  boundary: outer =>
    val y = 2
    boundary: inner =>
      val z = 3
      suspend(outer): () =>
        println(inner) // error  (leaks the inner label)
        x + y + z