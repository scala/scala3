class C:
  def meth(x: Short, y: Short): Unit = ()

object D:
  val c = C()
  c.meth(1, 2) // OK
