object S extends T:
  object X:
    def m: Int = 42

object D extends C:
  object X:
    def m: Int = 42

@main def Test =
  println(S.f)
  println(D.c)
