def f[T](xs: => T*): (Seq[T], Seq[T]) = (xs, xs)

def a =
  println("a")
  1

def b =
  println("b")
  2

@main def Test =
  println(s"result = ${f(a, b)}")