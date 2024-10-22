class C {
  def f = 42 + (x = 1) // error // a named tuple!
  def multi(x: Int, y: Int): Int = x + y
  def **(x: Int, y: Int): Int = x + y
  def g = new C() `multi` (x = 42, y = 27) // werror // werror // not actually a tuple! appearances to the contrary
  def h = new C() ** (x = 42, y = 27) // werror // werror
}
