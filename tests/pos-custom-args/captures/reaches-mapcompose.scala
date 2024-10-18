def compose1[A, B, C](f: A => B, g: B => C): A ->{f, g} C =
  z => g(f(z))

def mapCompose[A](ps: List[(A => A, A => A)]): List[A ->{ps*} A] =
  ps.map((x, y) => compose1(x, y)) // now ok

def mapCompose2[A](ps: List[(A => A, A => A)]): List[A ->{ps*} A] =
  ps.map((x, y) => compose1(x, y))
