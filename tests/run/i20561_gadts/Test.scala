def f[X](c: A[X]): X =
  c match
    case A.A1(x) =>
      x
    case A.A2(x) =>
      x

@main def Test() =
  val a1 = f(A.A1(2))
  println(a1)
  val a2 = f(A.A2(true))
  println(a2)
