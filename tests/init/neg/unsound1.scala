class A(m: Int) {
  if (m > 0) println(foo(m - 1).a2.n) // error
  def foo(n: Int): B =
    if (n % 2 == 0)
      new B(new A(n - 1), foo(n - 1).a1)
    else
      new B(this, new A(n - 1))
  var n: Int = 10
}

class B(val a1: A, val a2: A)