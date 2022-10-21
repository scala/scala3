trait Cap { def use(): Int }
type Id[X] = [T] -> (op: X => T) -> T
def mkId[X](x: X): Id[X] = [T] => (op: X => T) => op(x)

def foo(x: Id[{*} Cap]) = {
  x(_.use())  // error
}

def bar(io: {*} Cap, x: Id[{io} Cap]) = {
  x(_.use())
}

def barAlt(a: {*} Cap, b: {*} Cap, x: Id[{a, b} Cap]) = {
  x(_.use())
}
