class C:
  def meth(x: Long, y: Long): Unit = ()

object D:
  val c = C()
  c.meth(1, 2) // OK
  c.meth(1L, 2) // OK
