trait Cap { def use(): Int }
type Id[X] = [T] -> (op: X => T) -> T
def mkId[X](x: X): Id[X] = [T] => (op: X => T) => op(x)

def foo(x: Id[Cap^]) = {
  x(_.use())  // error, was OK under sealed policy
}
