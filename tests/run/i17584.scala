trait A:
  inline def g = 1
trait B extends A:
  def f = super.g
class C extends B

@main def Test =
  val c = C()
  assert(c.f == 1)
