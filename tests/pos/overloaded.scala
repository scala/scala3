object overloaded {

  def f(x: String): String = x
  def f[T >: Null](x: T): Int = 1
  
  val x1 = f("abc")
  val x2 = f(new Integer(1))
  val x3 = f(null)

}
