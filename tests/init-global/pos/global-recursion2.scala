class A(val a: A)

object B:
  val a: A = loop(ofA())
  def ofA(): A = ofA().a
  def loop(a: A): A = loop(new A(a))