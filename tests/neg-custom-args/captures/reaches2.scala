

class List[+A]:
  def map[B](f: A -> B): List[B] = ???

def compose1[A, B, C](f: A => B, g: B => C): A ->{f, g} C =
  z => g(f(z))

def mapCompose[A, c^](ps: List[(A ->{c} A, A ->{c} A)]): List[A ->{c} A] =
  ps.map((x, y) => compose1(x, y)) // error

