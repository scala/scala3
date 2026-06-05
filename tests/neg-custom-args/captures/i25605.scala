class IO
case class Id[X](unwrap: [R] -> (x: IO^, op: X => R) -> R)
def magic(): Id[IO^] = Id([R] => (x, op) => op(x))  // error

def foo[X](f: [R] -> (x: IO^, op: X => R) -> R): X = ???
def magic2(): IO^ = foo([R] => (x, op) => op(x)) // error