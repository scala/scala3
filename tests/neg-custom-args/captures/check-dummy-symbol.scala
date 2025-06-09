import language.experimental.captureChecking

class A:
  type C^

def f(a: A): a.C = a.C // error
def g[C^]: C = C // error