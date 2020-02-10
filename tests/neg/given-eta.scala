
trait D:
   type T
   def trans(other: T): T

def h(d: D)(using x: d.T)(y: d.T) = (d.trans(x), d.trans(y))

val z = h   // error: no implicit argument of type d.T was found for parameter x of method h

def f(using D)(x: Int) = x  // OK
