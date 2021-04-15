import scala.annotation.experimental

@experimental // error
val x = ()

@experimental // error
def f() = ()

@experimental // error
class A:
  def f() = 1

@experimental // error
class B extends A:
  override def f() = 2

@experimental // error
type X

@experimental // error
type Y = Int

@experimental // error
opaque type Z = Int

@experimental // error
object X:
  def fx() = 1

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

object Extractor1:
  def unapply(s: Any): Option[A] = ??? // error

object Extractor2:
  @experimental // error
  def unapply(s: Any): Option[Int] = ???


@experimental // error
trait ExpSAM {
  def foo(x: Int): Int
}
def bar(f: ExpSAM): Unit = {} // error

@experimental // error
enum E:
  case A
  case B

def test(
  p1: A, // error
  p2: List[A], // error
  p3: X, // error
  p4: Y, // error
  p5: Z, // error
): Unit =
  f() // error
  x // error
  new A // error
  new B // error
  X.fx() // error
  import X.fx // error
  fx() // error
  val i1 = identity[X] // error // error
  val i2 = identity[A] // error // error
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

  (??? : Any) match
    case _: A => // error // error
    case Extractor1(_) => // error
    case Extractor2(_) => // error

  bar(x => x) // error

  E.A // error
  E.B // error
  val e: E = ??? // error
