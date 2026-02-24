trait X[A]

given [A <: X[B], B <: A] => A => B = ???

type Z = {
  val v: Int
}

def f(z: Z) = z.v // error
