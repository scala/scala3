class C
def f(x: C @retains(*), y: C): () -> C =
  () => if x == null then y else y  // error

def g(x: C @retains(*), y: C): Matchable =
  () => if x == null then y else y  // error

def h1(x: C @retains(*), y: C): Any =
  def f() = if x == null then y else y
  () => f()  // ok

def h2(x: C @retains(*)): Matchable =
  def f(y: Int) = if x == null then y else y  // error
  f

class A
type Cap = C @retains(*)

def h3(x: Cap): A =
  class F(y: Int) extends A:   // error
    def m() = if x == null then y else y
  F(22)

def h4(x: Cap, y: Int): A =
  new A: // error
    def m() = if x == null then y else y

def foo() =
  val x: C @retains(*) = ???
  def h[X](a: X)(b: X) = a
  val z2 = h[() -> Cap](() => x)(() => C())  // error
  val z3 = h[(() -> Cap) @retains(x)](() => x)(() => C())  // ok
  val z4 = h[(() -> Cap) @retains(x)](() => x)(() => C())  // what was inferred for z3

