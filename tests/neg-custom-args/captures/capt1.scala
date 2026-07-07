
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
