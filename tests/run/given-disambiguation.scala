import language.experimental.modularity
import language.future

trait M extends ValueTypeClass:
  extension (x: Self) def combine (y: Self): String
  def unit: Self

trait Num extends ValueTypeClass:
  def zero: Self

trait A extends M
trait B extends M

def f[X: {M, A, B}](x: X) =
    summon[X is M]
    x.combine(x)

trait AA:
  type XX: {M, A, B}
  val x = XX.unit
  val A: String = "hello"

trait AAA:
  type X: M
trait BBB:
  type X: Num
class CCC[X1: {M, Num}] extends AAA, BBB:
  type X = X1
  X.zero
  X.unit

@main def Test =
  class C

  given C is M:
    extension (x: Self) def combine (y: Self) = "M"
    def unit = C()

  given C is A:
    extension (x: Self) def combine (y: Self) = "A"
    def unit = C()

  given C is B:
    extension (x: Self) def combine (y: Self) = "B"
    def unit = C()

  assert(f(C()) == "M")

  class CC extends AA:
    type XX = C
    assert(A.length == 5)
    assert(A.toString == "hello")

  CC()


