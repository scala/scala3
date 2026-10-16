//> using options -experimental

class C(val i: Int)

@main def Test(): Unit =
  given C = new C(1)
  given String = "s"

  val f23 = (
    a1: C, a2: C, a3: C, a4: C, a5: C, a6: C, a7: C, a8: C, a9: C, a10: C, a11: C, a12: C,
    a13: C, a14: C, a15: C, a16: C, a17: C, a18: C, a19: C, a20: C, a21: C, a22: C, a23: String) =>
    List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22).map(_.i).sum.toString + a23
  assert(f23.applyToContext == "22s")

  val f25 = (
    a1: C, a2: C, a3: C, a4: C, a5: C, a6: C, a7: C, a8: C, a9: C, a10: C, a11: C, a12: C, a13: C,
    a14: C, a15: C, a16: C, a17: C, a18: C, a19: C, a20: C, a21: C, a22: C, a23: C, a24: C, a25: C) =>
    List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25).map(_.i).sum
  assert(f25.applyToContext == 25)

  // functions of arity up to 22 still use the fixed-arity overloads
  assert(((c: C, s: String) => s * (c.i + 1)).applyToContext == "ss")
