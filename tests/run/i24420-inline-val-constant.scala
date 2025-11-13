inline def f1(): Long =
  1L

inline def f3(): Long =
  inline val x = f1()
  x

@main def Test: Unit =
  assert(f3() == 1L)
