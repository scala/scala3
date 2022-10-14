
type N = [X] => (X => X) => X => X
val exp = (a: N) => (b: N) => b(a) // error
