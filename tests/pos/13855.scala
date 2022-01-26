type A[X] = X match
  case Int => Int
  case _   => B[X]

def a[X](x: X): A[X] = x match
  case v: Int => v
  case _      => b(x)

type B[X] = X match
  case String => String

def b[X](x: X): B[X] = x match
  case v: String => v
