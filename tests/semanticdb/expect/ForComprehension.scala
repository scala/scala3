package example

class ForComprehension {
  for {
    a <- List(1)
    b <- List(1)
    if b > 1
    c = a + b
  } yield (a, b, c)
  for {
    a <- List(1)
    b <- List(a)
    if (
      a,
      b
    ) == (1, 2)
    (
      c,
      d
    ) <- List((a, b))
    if (
      a,
      b,
      c,
      d
    ) == (1, 2, 3, 4)
    e = (
      a,
      b,
      c,
      d
    )
    if e == (1, 2, 3, 4)
    f <- List(e)
  } yield {
    (
      a,
      b,
      c,
      d,
      e,
      f
    )
  }
}
