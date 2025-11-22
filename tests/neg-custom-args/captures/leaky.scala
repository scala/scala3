package test.runnable
import language.experimental.captureChecking

case class A() extends caps.SharedCapability:
  def print() = println("leaking...")

class Transform(fun: Any => Any):
  def run() = fun(())
object Transform:
  def app(f: Any => Any): Transform { val fun: Any->{f} Any } ^ {f} =
    Transform(f)

def leak(a: A): Transform^{} =
  val f: Any ->{a} Any = _ =>
    a.print()
    ()
  Transform(f)  // error

def leak1(a: A): Transform^{} =
  val f: Any ->{a} Any = _ =>
    a.print()
    ()
  val x = Transform(f)
  x  // error

def leak2(a: A): Transform^{} =
  val f: Any ->{a} Any = _ =>
    a.print()
    ()
  val x = Transform.app(f)
  x  // error

def withA[T](body: A => T): T = body(A())

@main def Main() =
  val t = withA(leak)
  t.run()
