import annotation.tailrec

object i1221{
  final def foo(a: Int): Int = {
  if (foo(a - 1) > 0) 
    foo(a - 1): @tailrec 
  else 
    foo(a - 2): @tailrec
  }
}
