class C:
  def meth(x: Float, y: Float): Unit = ()

object D:
  val c = C()
  c.meth(1.0, 2.0) // OK
  c.meth(1.0f, 2.0) // OK
