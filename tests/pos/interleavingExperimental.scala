//> using options --source 3.6

def ba[A](x: A)[B](using B): B = summon[B]