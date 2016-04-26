import annotation.tailrec

object I1221{
  final def foo(a: Int): Int = {
  if ((foo(a - 1): @tailrec) > 0) // error: not in tail position
    foo(a - 1): @tailrec
  else
    foo(a - 2): @tailrec
  }
}
