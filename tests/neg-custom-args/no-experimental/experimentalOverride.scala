import scala.annotation.experimental

@experimental // error
class A:
  def f() = 1

@experimental // error
class B extends A:
  override def f() = 2

class C:
  @experimental // error
  def f() = 1

class D extends C:
  override def f() = 2

trait A2:
  @experimental // error
  def f(): Int

trait B2:
  def f(): Int

class C2 extends A2, B2:
  def f(): Int = 1

def test: Unit =
  val a: A = ??? // error
  val b: B = ??? // error
  val c: C = ???
  val d: D = ???
  val c2: C2 = ???
  a.f() // error
  b.f() // error
  c.f() // error
  d.f() // ok because D.f is a stable API
  c2.f() // ok because B2.f is a stable API
  ()
