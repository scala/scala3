class Box[T](val value: T)

class A[T](var a: T):
  def update(n: T) =
    a = n

class B[T](var b: T):
  def update(n: T) =
    b = n

object O:
  f(foo(), bar())

  def f(a: Box[A[Int]], b: Box[B[Int]]): Unit =
    h(a.value, b.value)

  def h(a: A[Int], b: B[Int]): Unit =
    b.update(5)
    a.update(10)

  def foo(): Box[A[Int]] = Box(A(3))
  def bar(): Box[B[Int]] = Box(B(4))
