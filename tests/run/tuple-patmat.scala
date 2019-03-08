case class Box(t: (String, Int, A))
case class A(i: Int, j: Int)

object Test {
  def main(args: Array[String]) = {
    val x1 = Box(("a", 2, A(3, 4)))

    x1 match {
      case Box((a3, b3, A(a1, a2))) =>
        a3: String
        b3: Int
        a1: Int
        a2: Int
        assert(a3 == "a")
        assert(b3 == 2)
        assert(a1 == 3)
        assert(a2 == 4)
    }

    val x2 = ("a", 2)

    x2 match {
      case (a2, b2) =>
        a2: String
        b2: Int
        assert(a2 == "a")
        assert(b2 == 2)
    }

    val x3 = ("a", 2, "c")

    x3 match {
      case (a3, b3, c3) =>
        a3: String
        b3: Int
        c3: String
        assert(a3 == "a")
        assert(b3 == 2)
        assert(c3 == "c")
    }

    val x4 = ("a", 2, "c", 4)

    x4 match {
      case (a4, b4, c4, d4) =>
        a4: String
        b4: Int
        c4: String
        d4: Int
        assert(a4 == "a")
        assert(b4 == 2)
        assert(c4 == "c")
        assert(d4 == 4)
    }

    val x5 = ("a", 2, "c", 4, "e")

    x5 match {
      case (a5, b5, c5, d5, e5) =>
        a5: String
        b5: Int
        c5: String
        d5: Int
        e5: String
        assert(a5 == "a")
        assert(b5 == 2)
        assert(c5 == "c")
        assert(d5 == 4)
        assert(e5 == "e")
    }

    val x6 = ("a", 2, "c", 4, "e", 6)

    x6 match {
      case (a6, b6, c6, d6, e6, f6) =>
        a6: String
        b6: Int
        c6: String
        d6: Int
        e6: String
        f6: Int
        assert(a6 == "a")
        assert(b6 == 2)
        assert(c6 == "c")
        assert(d6 == 4)
        assert(e6 == "e")
        assert(f6 == 6)
    }

    val x21 = ("a", 2, "c", 4, "e", 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, "21")

    x21 match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) =>
        a1: String
        a2: Int
        a3: String
        a4: Int
        a5: String
        a6: Int
        a7: Int
        a8: Int
        a9: Int
        a10: Int
        a11: Int
        a12: Int
        a13: Int
        a14: Int
        a15: Int
        a16: Int
        a17: Int
        a18: Int
        a19: Int
        a20: Int
        a21: String
        assert(a1 == "a")
        assert(a2 == 2)
        assert(a3 == "c")
        assert(a4 == 4)
        assert(a5 == "e")
        assert(a6 == 6)
        assert(a7 == 7)
        assert(a8 == 8)
        assert(a9 == 9)
        assert(a10 == 10)
        assert(a11 == 11)
        assert(a12 == 12)
        assert(a13 == 13)
        assert(a14 == 14)
        assert(a15 == 15)
        assert(a16 == 16)
        assert(a17 == 17)
        assert(a18 == 18)
        assert(a19 == 19)
        assert(a20 == 20)
        assert(a21 == "21")
    }

    val x22 = ("a", 2, "c", 4, "e", 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, "21", 22)

    x22 match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) =>
        a1: String
        a2: Int
        a3: String
        a4: Int
        a5: String
        a6: Int
        a7: Int
        a8: Int
        a9: Int
        a10: Int
        a11: Int
        a12: Int
        a13: Int
        a14: Int
        a15: Int
        a16: Int
        a17: Int
        a18: Int
        a19: Int
        a20: Int
        a21: String
        a22: Int
        assert(a1 == "a")
        assert(a2 == 2)
        assert(a3 == "c")
        assert(a4 == 4)
        assert(a5 == "e")
        assert(a6 == 6)
        assert(a7 == 7)
        assert(a8 == 8)
        assert(a9 == 9)
        assert(a10 == 10)
        assert(a11 == 11)
        assert(a12 == 12)
        assert(a13 == 13)
        assert(a14 == 14)
        assert(a15 == 15)
        assert(a16 == 16)
        assert(a17 == 17)
        assert(a18 == 18)
        assert(a19 == 19)
        assert(a20 == 20)
        assert(a21 == "21")
        assert(a22 == 22)
    }

    val x23 = ("a", 2, "c", 4, "e", 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, "21", 22, "23")

    x23 match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23) =>
        a1: String
        a2: Int
        a3: String
        a4: Int
        a5: String
        a6: Int
        a7: Int
        a8: Int
        a9: Int
        a10: Int
        a11: Int
        a12: Int
        a13: Int
        a14: Int
        a15: Int
        a16: Int
        a17: Int
        a18: Int
        a19: Int
        a20: Int
        a21: String
        a22: Int
        a23: String
        assert(a1 == "a")
        assert(a2 == 2)
        assert(a3 == "c")
        assert(a4 == 4)
        assert(a5 == "e")
        assert(a6 == 6)
        assert(a7 == 7)
        assert(a8 == 8)
        assert(a9 == 9)
        assert(a10 == 10)
        assert(a11 == 11)
        assert(a12 == 12)
        assert(a13 == 13)
        assert(a14 == 14)
        assert(a15 == 15)
        assert(a16 == 16)
        assert(a17 == 17)
        assert(a18 == 18)
        assert(a19 == 19)
        assert(a20 == 20)
        assert(a21 == "21")
        assert(a22 == 22)
        assert(a23 == "23")
    }

    val x24 = ("a", 2, "c", 4, "e", 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, "21", 22, "23", 24)

    x24 match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24) =>
        a1: String
        a2: Int
        a3: String
        a4: Int
        a5: String
        a6: Int
        a7: Int
        a8: Int
        a9: Int
        a10: Int
        a11: Int
        a12: Int
        a13: Int
        a14: Int
        a15: Int
        a16: Int
        a17: Int
        a18: Int
        a19: Int
        a20: Int
        a21: String
        a22: Int
        a23: String
        a24: Int
        assert(a1 == "a")
        assert(a2 == 2)
        assert(a3 == "c")
        assert(a4 == 4)
        assert(a5 == "e")
        assert(a6 == 6)
        assert(a7 == 7)
        assert(a8 == 8)
        assert(a9 == 9)
        assert(a10 == 10)
        assert(a11 == 11)
        assert(a12 == 12)
        assert(a13 == 13)
        assert(a14 == 14)
        assert(a15 == 15)
        assert(a16 == 16)
        assert(a17 == 17)
        assert(a18 == 18)
        assert(a19 == 19)
        assert(a20 == 20)
        assert(a21 == "21")
        assert(a22 == 22)
        assert(a23 == "23")
        assert(a24 == 24)
    }
  }
}
