import language.experimental.captureChecking
import caps.*

trait CF1 extends Classifier, SharedCapability
trait CF2 extends CF1, Classifier

class C:
  class Impl(val a: C^{C.this}) extends CF2 // error
  val cf1: CF1 = ???
  val cf2 = new Impl(this)

def f(x: AnyRef^{any.only[CF2]}): Unit = ()

def test(c: C): Unit =
  f(c.cf1) // error
  f(c.cf2) // ok
  f(c.cf2.a.cf1) // error
  val d: AnyRef^{c.cf2} = c.cf2.a.cf1
  f(d) // wrong, passing CF1 to CF2 only

