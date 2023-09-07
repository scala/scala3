trait Cap { def use(): Int }
type Id[X] = [T] -> (op: X => T) -> T
def mkId[X](x: X): Id[X] = [T] => (op: X => T) => op(x)

def foo(x: Id[Cap^{cap}]) = {
  x(_.use())  // was error, now OK
}

def bar(io: Cap^{cap}, x: Id[Cap^{io}]) = {
  x(_.use())
}

def barAlt(a: Cap^{cap}, b: Cap^{cap}, x: Id[Cap]^{a, b}) = {
  x(_.use())
}
