//> using options -deprecation

trait A:
  def f: Int
  def g(): Unit

class B extends A:
  @deprecatedOverriding def f = 1
  @deprecatedOverriding("Please don't.", since = "now")
  def g() = println("hello, world")

class C extends B:
  override def f = 2  // warn
  override def g() = println("goodbye, cruel world") // warn

trait D extends A:
  override def f = 3
  override def g() = ()

object E extends B, D  // warn

@deprecated
class F extends B:
  override def g() = println("goodbye, cruel world") // nowarn

trait X[T]:
  @deprecatedOverriding def f: T

class Y extends X[String]:
  override def f = "hello, world" // warn
