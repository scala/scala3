transparent inline def f(): Long =
  1L

transparent inline def g(): Long =
  inline val x = f()
  x

transparent inline def h(): Long =
  inline if g() > 0L then 1L else 0L

@main def Test: Unit =
  assert(h() == 1L)
