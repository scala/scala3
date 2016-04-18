import annotation.tailrec

class Test {
  def foo(a: Int): Int = { // error: method is not final
    if ((foo(a - 1): @tailrec) > 0)
      foo(a - 1): @tailrec
    else
      foo(a - 2): @tailrec
    }
}
