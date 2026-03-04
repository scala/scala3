
def f(@deprecatedName("x") x: Int, @deprecatedName y: Int, @deprecatedName("w") z: Int) = x+y+z

def g(@deprecatedName("a") x: Int, a: Int) = x+a // error

def h(@deprecatedName("a") x: Int, @deprecatedName("a") y: Int) = x+y // error

def i(x: Int, @deprecatedName("x") y: Int) = x+y // error

def j(x: Int, @deprecatedName("x") x: Int) = x // error

@main def Test =
  f(1, 2, 3, x = 42) // error
  f(1, 2, 3, w = 42) // error
  f(1, 2, w = 42, z = 27) // error
  f(1, 2, z = 42, w = 27) // error

object X { def m[T](i: Int)(s: String) = s*i; def m[T](i: Int)(d: Double) = d*i }

object Y:
  def f = X.m(42)(s = "") // overloading resolution objects to methRef.symbol.paramSymss
