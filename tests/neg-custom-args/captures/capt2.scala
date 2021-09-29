//import scala.retains
class C
type Cap = {*} C

def f1(c: Cap): (() => {c} C) = () => c // error, but would be OK under capture abbreciations for funciton types
def f2(c: Cap): ({c} () => C) = () => c // error

def h5(x: Cap): () => C =
  f1(x)  // error
