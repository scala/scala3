import language.experimental.captureChecking
import caps.*

trait Ctx extends SharedCapability

class C[T]:
  def value(using Ctx): T = ???

trait F1[T]:
  def f(x: C[T]): T

trait F2[T]:
  self =>
  def f(x: C[T]): T

object C:
  given [T](using ctx: Ctx): (Conversion[C[T], T]^{ctx}) = _.value // error

def getF1[T](using ctx: Ctx): F1[T]^{ctx} = new F1[T]:
  def f(c: C[T]): T = c.value

def getF2[T](using ctx: Ctx): F2[T]^{ctx} = new F2[T]:
  def f(c: C[T]): T = c.value // error

def test(using Ctx): Unit =
  val c = new C[Int]
  val n1: Int = c
  val n2: Int = getF1.f(c)
  val n3: Int = getF2.f(c)

