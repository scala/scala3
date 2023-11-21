trait T:
  def g = 2
trait A extends T:
  inline override def g = 1
trait B extends A:
  def f = super.g
class C extends B

@main def Test =
  val c = C()
  assert(c.f == 1)