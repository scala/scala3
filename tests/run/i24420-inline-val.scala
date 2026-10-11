inline def f1(): Long =
  1L

inline def f2(): Long =
  inline val x = f1() + 1L
  x

inline def f3(): Long =
  inline val x = f1()
  x

inline def g1(): Boolean =
  true

inline def g2(): Long =
  inline if g1() then 1L else 2L

inline def g3(): Long =
  inline if f1() > 0L then 1L else 2L

@main def Test: Unit =
  assert(f2() == 2L)
  assert(f3() == 1L)
  assert(g2() == 1L)
  assert(g3() == 1L)
