//> using options -Werror -deprecation

def f(x: Int, @deprecatedName("huh") y: Int) = x + y

def i = f(`z` = 42, x = 27) // error: bq means positional so x is already used
def j = f(`z` = 42, `z` = 27) // error: bq must be unique
def k = f(x = 42, `huh` = 27) // ok because x is in position, but warns deprecation
def m = f(x = 42, `z` = 27) // ok because x is in position

def g(x: Int, y: Int, @deprecatedName("huh") z: Int) = x + y + z

def n = g(y = 27, x = 42, `z` = 0) // error: positional
def p = g(y = 27, x = 42, `huh` = 0) // error: positional
def q = g(x = 27, `z` = 42, 0) // error: must be unique
// nopos-error
