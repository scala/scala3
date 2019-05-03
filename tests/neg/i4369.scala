trait X[R <: Z, Z >: X[R, R]] {
   def foo(x: X[R, R]): Z = x // error: Found: X[R, R](x)    Required: Z
}
class Z extends X[Z, Z]
