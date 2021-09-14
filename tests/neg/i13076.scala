trait T[A, B] {
  def f(a: A): B
  def g(a: A, b: B): Unit
}

class C[X, Y] extends T[X, Y] {} // error
