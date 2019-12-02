trait X[R <: Z, Z >: X[R, R]] // error
class Z extends X[Z, Z]
