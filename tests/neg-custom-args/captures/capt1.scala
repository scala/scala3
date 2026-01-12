
import annotation.retains
class C
def f(x: C @retains[caps.any.type], y: C): () -> C =
  () => if x == null then y else y  // error

def g(x: C @retains[caps.any.type], y: C): Matchable =
  () => if x == null then y else y  // error

def h1(x: C @retains[caps.any.type], y: C): Any =
  def f() = if x == null then y else y
  () => f()  // ok

def h2(x: C @retains[caps.any.type]): Matchable =
  def f(y: Int) = if x == null then y else y
  f  // error

class A
type Cap = C @retains[caps.any.type]

def h3(x: Cap): A =
  class F(y: Int) extends A:
    def m() = if x == null then y else y
  F(22)  // error

def h4(x: Cap, y: Int): A =
  new A: // error
    def m() = if x == null then y else y

def f1(c: Cap): () ->{c} c.type = () => c // ok

def foo() =
  val x: C @retains[caps.any.type] = ???
  def h[X](a: X)(b: X) = a

  val z2 = h[() -> Cap](() => x) // error
    (() => C()) // error
  val z3 = h[(() -> Cap) @retains[x.type]](() => x)(() => C())  // error

  val z1: () => Cap = f1(x) // error

  val z4 =
    if x == null then  // error: separation
      () => x  // error: separation
    else
      () => C()
  x // error: separation
