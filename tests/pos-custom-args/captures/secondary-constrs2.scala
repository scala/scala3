import language.experimental.captureChecking

class T extends caps.SharedCapability

class A(val x: T, val y: T):
  def this(v: T) = this(v, v)

def f(x: T^) =
  val a = A(x)
  val b: T^{x} = a.x
  val c: T^{x} = a.y