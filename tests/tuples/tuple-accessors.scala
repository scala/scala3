import dotty._

case class X2[A, B](a: A, b: B)
case class X3[A, B, C](a: A, b: B, c: C)
case class X4[A, B, C, D](a: A, b: B, c: C, d: D)
case class X5[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E)
case class X6[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F)

case class X21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](e1: T1, e2: T2, e3: T3, e4: T4, e5: T5, e6: T6, e7: T7, e8: T8, e9: T9, e10: T10, e11: T11, e12: T12, e13: T13, e14: T14, e15: T15, e16: T16, e17: T17, e18: T18, e19: T19, e20: T20, e21: T21)

case class X22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](e1: T1, e2: T2, e3: T3, e4: T4, e5: T5, e6: T6, e7: T7, e8: T8, e9: T9, e10: T10, e11: T11, e12: T12, e13: T13, e14: T14, e15: T15, e16: T16, e17: T17, e18: T18, e19: T19, e20: T20, e21: T21, e22: T22)

case class X23[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23](e1: T1, e2: T2, e3: T3, e4: T4, e5: T5, e6: T6, e7: T7, e8: T8, e9: T9, e10: T10, e11: T11, e12: T12, e13: T13, e14: T14, e15: T15, e16: T16, e17: T17, e18: T18, e19: T19, e20: T20, e21: T21, e22: T22, e23: T23)

case class X24[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24](e1: T1, e2: T2, e3: T3, e4: T4, e5: T5, e6: T6, e7: T7, e8: T8, e9: T9, e10: T10, e11: T11, e12: T12, e13: T13, e14: T14, e15: T15, e16: T16, e17: T17, e18: T18, e19: T19, e20: T20, e21: T21, e22: T22, e23: T23, e24: T24)

object Test {
  def main(args: Array[String]) = {

    val x2 = X2("a", 2)
    val X2(a2, b2) = x2

    assert(a2 == "a")
    assert(b2 == 2)
    a2: String
    b2: Int

    val x3 = X3("a", 2, "b")
    val X3(a3, b3, c3) = x3

    assert(a3 == "a")
    assert(b3 == 2)
    assert(c3 == "b")
    b3: Int
    c3: String

    val x4 = X4("a", 2, "b", 3)
    val X4(a4, b4, c4, d4) = x4

    assert(a4 == "a")
    assert(b4 == 2)
    assert(c4 == "b")
    assert(d4 == 3)
    c4: String
    d4: Int

    val x5 = X5("a", 2, "b", 3, "c")
    val X5(a5, b5, c5, d5, e5) = x5

    assert(a5 == "a")
    assert(b5 == 2)
    assert(c5 == "b")
    assert(d5 == 3)
    assert(e5 == "c")
    d5: Int
    e5: String

    val x6 = X6("a", 2, "b", 3, "c", 4)
    val X6(a6, b6, c6, d6, e6, f6) = x6

    val x21 = X21(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
    val X21(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20, w21) = x21

    assert(w1 == 1)
    assert(w2 == 2)
    assert(w3 == 3)
    assert(w4 == 4)
    assert(w5 == 5)
    assert(w6 == 6)
    assert(w7 == 7)
    assert(w8 == 8)
    assert(w9 == 9)
    assert(w10 == 10)
    assert(w11 == 11)
    assert(w12 == 12)
    assert(w13 == 13)
    assert(w14 == 14)
    assert(w15 == 15)
    assert(w16 == 16)
    assert(w17 == 17)
    assert(w18 == 18)
    assert(w19 == 19)
    assert(w20 == 20)
    assert(w21 == 21)

    w1: Int
    w2: Int
    w20: Int
    w21: Int

    val x22 = X22(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    val X22(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22) = x22

    assert(v1 == 1)
    assert(v2 == 2)
    assert(v3 == 3)
    assert(v4 == 4)
    assert(v5 == 5)
    assert(v6 == 6)
    assert(v7 == 7)
    assert(v8 == 8)
    assert(v9 == 9)
    assert(v10 == 10)
    assert(v11 == 11)
    assert(v12 == 12)
    assert(v13 == 13)
    assert(v14 == 14)
    assert(v15 == 15)
    assert(v16 == 16)
    assert(v17 == 17)
    assert(v18 == 18)
    assert(v19 == 19)
    assert(v20 == 20)
    assert(v21 == 21)
    assert(v22 == 22)

    v1: Int
    v2: Int
    v21: Int
    v22: Int

    // We can't go above that since patDef is implemented with direct _i calls.
    // It should be possible to update the desugaring to use var + pattern
    // matching instead, which would lift this (the last?) 22 limitation.

    // val x23 = X23(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
    // val X23(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23) = x23

    // assert(y1 == 1)
    // assert(y2 == 2)
    // assert(y3 == 3)
    // assert(y4 == 4)
    // assert(y5 == 5)
    // assert(y6 == 6)
    // assert(y7 == 7)
    // assert(y8 == 8)
    // assert(y9 == 9)
    // assert(y10 == 10)
    // assert(y11 == 11)
    // assert(y12 == 12)
    // assert(y13 == 13)
    // assert(y14 == 14)
    // assert(y15 == 15)
    // assert(y16 == 16)
    // assert(y17 == 17)
    // assert(y18 == 18)
    // assert(y19 == 19)
    // assert(y20 == 20)
    // assert(y21 == 21)
    // assert(y22 == 22)
    // assert(y23 == 23)

    // y1: Int
    // y2: Int
    // y22: Int
    // y23: Int

    // val x24 = X24(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
    // val X24(z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16, z17, z18, z19, z20, z21, z22, z23, z24) = x24

    // assert(z1 == 1)
    // assert(z2 == 2)
    // assert(z3 == 3)
    // assert(z4 == 4)
    // assert(z5 == 5)
    // assert(z6 == 6)
    // assert(z7 == 7)
    // assert(z8 == 8)
    // assert(z9 == 9)
    // assert(z10 == 10)
    // assert(z11 == 11)
    // assert(z12 == 12)
    // assert(z13 == 13)
    // assert(z14 == 14)
    // assert(z15 == 15)
    // assert(z16 == 16)
    // assert(z17 == 17)
    // assert(z18 == 18)
    // assert(z19 == 19)
    // assert(z20 == 20)
    // assert(z21 == 21)
    // assert(z22 == 22)
    // assert(z23 == 23)
    // assert(z24 == 24)

    // z1: Int
    // z2: Int
    // z23: Int
    // z24: Int
  }

  def any = {
    val x: Any = null
    val X2(a2, b2) = x
    val X3(a3, b3, c3) = x
    val X4(a4, b4, c4, d4) = x
    val X5(a5, b5, c5, d5, e5) = x
    val X6(a6, b6, c6, d6, e6, f6) = x
    val X21(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20, w21) = x
    val X22(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) = x
    // val X23(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23) = x
    // val X24(z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16, z17, z18, z19, z20, z21, z22, z23, z24) = x
  }
}
