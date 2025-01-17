trait Cap { def use(): Int }
type Id[X] = [T] -> (op: X => T) -> T
def mkId[X](x: X): Id[X] = [T] => (op: X => T) => op(x)

def bar(io: Cap^, x: Id[Cap^{io}]) = {
  x(_.use())
}

def barAlt(a: Cap^, b: Cap^, x: Id[Cap]^{a, b}) = {
  x(_.use())
}
