package p
import language.experimental.captureChecking

class File

// The type argument of `indexOf` and `contains` does not appear in their result types,
// so it is not taken from the type of the closure, whose bound and formals do not
// conform to those of the lower bound
def bound(x: File^, l: List[[C^ <: {x}] -> (f: File^{C}) -> File^{C}]) =
  val n = l.indexOf([C^] => (f: File^{C}) => f)

def formal(l: List[[C^] -> (f: File) -> File]) =
  val b = l.contains([C^] => (f: File^{C}) => f)
