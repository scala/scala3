object Constants {

  def f[C <: Constant](c: C): Unit = ()
  f(true)
  f(1)
  f(2L)
  f(1.2f)
  f(1.2d)
  f('a')
  f("abc")

  def fBool(c: Boolean & Constant): Unit = ()
  fBool(true)
  fBool(false)
  transparent def bb: Boolean = true
  fBool(bb)

  def fInt(c: Int & Constant): Unit = ()
  fInt(1)
  fInt(2)
  transparent def ii: Int = 4
  fInt(ii)

  def fChar(c: Char & Constant): Unit = ()
  fChar('a')
  fChar('b')
  transparent def cc: Char = 'd'
  fChar(cc)

}
