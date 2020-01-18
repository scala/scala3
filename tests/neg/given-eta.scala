
trait D
   type T
   def trans(other: T): T

def h(d: D) with (x: d.T) (y: d.T) = (d.trans(x), d.trans(y)) // error: normal parameter cannot follow context parameter

val z = h   // error: no implicit argument of type d.T was found for parameter x of method h

