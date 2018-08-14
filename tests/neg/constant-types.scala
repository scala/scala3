object Constants {

  def f[C <: Constant](c: C): Unit = ()
  val x = 1
  f(x) // error

  def fBool(c: Boolean & Constant): Unit = ()
  val b = true
  fBool(b) // error


  def fInt(c: Int & Constant): Unit = ()
  val i = 3
  fInt(i) // error

  def fChar(c: Char & Constant): Unit = ()
  val c = 'c'
  fChar(c) // error

}
