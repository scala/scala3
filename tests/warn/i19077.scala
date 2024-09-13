//> using options -deprecation

def f(@deprecatedName("x") x: Int, @deprecatedName y: Int, @deprecatedName("w") z: Int) = x+y+z

@main def Test =
  f(1, 2, 3) // nowarn
  f(1, 2, z = 3) // nowarn
  f(x = 1, 2, 3) // warn
  f(1, y = 2, 3) // warn
  f(1, 2, w = 3) // warn
  f(w = 3, x = 1, y = 2) // warn // warn // warn
