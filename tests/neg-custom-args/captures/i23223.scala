import language.experimental.captureChecking
import caps.*

class A:
  def a: A = this

class B extends A, Capability // error

def leak(b: B): A = b.a

class C extends Capability:
  def c: C^{} = this // error
