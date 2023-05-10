trait Cap { def use(): Int }
type Id[X] = [T] -> (op: X => T) -> T
def mkId[X](x: X): Id[X] = [T] => (op: X => T) => op(x)

def foo(x: Id[Cap^{cap}]) = {
  x(_.use())  // error
}

def bar(io: Cap^{cap}, x: Id[Cap^{io}]) = {
  x(_.use())
}

def barAlt(a: Cap^{cap}, b: Cap^{cap}, x: Id[Cap]^{a, b}) = {
  x(_.use())
}
