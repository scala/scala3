def f(
  @deprecatedName("x".toUpperCase) a: Int // error
): Int = a

def g(
  @deprecatedName(Symbol("x".toUpperCase)) a: Int // error
): Int = a

def h(
  @deprecatedName("x" + 1) a: Int // error
): Int = a

def i(
  @deprecatedName(Symbol("x" + 1)) a: Int // error
): Int = a

