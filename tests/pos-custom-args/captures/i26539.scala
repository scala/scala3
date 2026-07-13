class Ref

class Abs[A](val fst: A^, val snd: A^)

def mkAbs[A](consume x: A^, consume y: A^): Abs[A]^ = Abs[A](x, y) // ok

def mkAbs2[A](consume x: A^, consume y: A^): Abs[A]^ = Abs(x, y) // !!! error, because A^{x, y} is inferred as type parameter for `Abs`
