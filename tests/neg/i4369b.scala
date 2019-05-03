trait X[R <: Z, Z >: X[R, R] <: X[R, R]] {
  implicitly[Z =:= X[R, R]] // error:  Cannot prove that Z =:= X[R, R]
}
class Z extends X[Z, Z]
