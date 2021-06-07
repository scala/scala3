def f1(c: Cap): (() => C holds c.type) = () => c // ok
def f2(c: Cap): (() => C) holds c.type = () => c // error

def h5(x: Cap): () => C =
  f1(x)  // error
