import language.future

def test2(a: A, b: B, c: C, d: D): Unit =
  a x 1 // ok: was compiled with 3.0
  b x 1 // ok: was compiled with 3.1
  c x 1 // ok: was compiled with 3.2
  d x 1 // ok: was compiled with 3.3

  // ok: is marked as infix
  a y 2
  b y 2
  c y 2
  d y 2
