import language.experimental.captureChecking
import caps.*

trait A extends Classifier, SharedCapability
trait B extends A, Classifier
trait E

def test[C <: B, D <: Classifier] =
  val a: AnyRef^{any.only[A]} = new AnyRef
  val b: AnyRef^{any.only[B]} = new AnyRef
  val c: AnyRef^{any.only[C]} = new AnyRef // error
  val d: AnyRef^{any.only[D]} = new AnyRef // error
  val e: AnyRef^{any.only[E]} = new AnyRef // error
