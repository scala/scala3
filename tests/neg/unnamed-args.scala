
def s = "abacus"

def i = s.indexOf(`n` = 'a', `n` = 1) // error

def j = s.indexOf('a', start = 1) // error

object X:
  def f(s: String) = s.length
  def f(i: Int) = i

def k = X.f(n = 42) // error

def m = X.f(s = 42) // error
