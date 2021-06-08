class C
def f(x: C holds *, y: C): () => C =
  () => if x == null then y else y  // error

def g(x: C holds *, y: C): Any =
  () => if x == null then y else y  // error

def h1(x: C holds *, y: C): Any holds x.type =
  def f() = if x == null then y else y
  () => f()  // ok

def h2(x: C holds *): Any =
  def f(y: Int) = if x == null then y else y
  f  // error

class A
type Cap = C holds *
type Top = Any holds *

def h3(x: Cap): A =
  class F(y: Int) extends A:
    def m() = if x == null then y else y
  F(22)  // error

def h4(x: Cap, y: Int): A =
  new A:
    def m() = if x == null then y else y  // error

def foo() =
  val x: C holds * = ???
  def h[X <:Top](a: X)(b: X) = a
  val z2 = h[() => Cap](() => x)(() => C())  // error
  val z3 = h(() => x)(() => C())  // error

