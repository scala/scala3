trait A:
  private def f: Int = 1
  inline def g = f
trait B:
  private def f: Int = 1
  inline def h = f
class C extends A, B


