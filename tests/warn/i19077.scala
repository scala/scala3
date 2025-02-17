//> using options -deprecation

def f(@deprecatedName("x") x: Int, @deprecatedName y: Int, @deprecatedName("w") z: Int) = x+y+z

object X:
  def f(@deprecatedName("v", since="3.3") x: Int) = x
  def f(@deprecatedName("v") x: Int, y: Int) = x+y

@main def Test =
  f(1, 2, 3) // nowarn
  f(1, 2, z = 3) // nowarn
  f(x = 1, 2, 3) // warn
  f(1, y = 2, 3) // warn
  f(1, 2, w = 3) // warn
  f(x = 1, w = 3, y = 2) // warn // warn // warn
  f(w = 3, x = 1, y = 2) // warn // warn // warn

  X.f(42)
  X.f(x = 42)
  X.f(v = 42) // warn
  X.f(x = 42, y = 27)
  X.f(y = 42, x = 27)

object empty:
  def f(@deprecatedName("y", since="") x: Int) = x
  def g = f(y = 42) // warn but omit empty since
