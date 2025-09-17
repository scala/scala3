import language.experimental.captureChecking
import caps.*

class A:
  def a: A = this

class B extends A, SharedCapability // error

def leak(b: B): A = b.a

class C extends SharedCapability:
  def c: C^{} = this // error
