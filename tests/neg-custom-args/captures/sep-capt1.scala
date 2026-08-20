
import annotation.retains
class C
class A
type Cap = C @retains[caps.any.type]

def f1(c: Cap): () ->{c} c.type = () => c // ok

def foo() =
  val x: C @retains[caps.any.type] = ???
  def h[X](a: X)(b: X) = a

  val z2 = h[() -> Cap](() => x)
    (() => C())
  val z3 = h[(() -> Cap) @retains[x.type]](() => x)(() => C())  // error

  val z1: () => Cap = f1(x) // error

  val z4 =
    if x == null then  // error: separation
      () => x  // error: separation
    else
      () => C()
  x // error: separation
