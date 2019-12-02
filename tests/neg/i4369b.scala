trait X[R <: Z, Z >: X[R, R] <: X[R, R]] // error // error
class Z extends X[Z, Z]
