object S extends T:
  object X:
    def m: Int = 42

object D extends C:
  object Y:
    def n: Int = 27

@main def Test =
  assert(S.f == 42)
  assert(D.c == 27)
