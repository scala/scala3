//> using options -source 3.4
// (to make sure we use the sealed policy)
import annotation.retains
class C
def f(x: C @retains(caps.cap), y: C): () -> C =
  () => if x == null then y else y  // error

def g(x: C @retains(caps.cap), y: C): Matchable =
  () => if x == null then y else y  // error

def h1(x: C @retains(caps.cap), y: C): Any =
  def f() = if x == null then y else y
  () => f()  // ok

def h2(x: C @retains(caps.cap)): Matchable =
  def f(y: Int) = if x == null then y else y  // error
  f

class A
type Cap = C @retains(caps.cap)

def h3(x: Cap): A =
  class F(y: Int) extends A:   // error
    def m() = if x == null then y else y
  F(22)

def h4(x: Cap, y: Int): A =
  new A: // error
    def m() = if x == null then y else y

def foo() =
  val x: C @retains(caps.cap) = ???
  def h[X](a: X)(b: X) = a
  val z2 = h[() -> Cap](() => x) // error // error
    (() => C())
  val z3 = h[(() -> Cap) @retains(x)](() => x)(() => C())  // error

