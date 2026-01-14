object S extends T:
  object X:
    def m: Int = 42

@main def Test =
  assert(S.f == 42)
