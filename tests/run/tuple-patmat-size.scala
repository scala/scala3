object Test {
  def main(args: Array[String]) = {
    val x2 = ("a", 2)
    val x3 = ("a", 2, "c")
    val x4 = ("a", 2, "c", 3)
    val x5 = ("a", 2, "c", 3, "e")
    val x21 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
    val x22 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    val x23 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
    val x24 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

    (x2: Any) match {
      case (a2, b2) =>
        assert(a2 == "a")
        assert(b2 == 2)
      case (a3, b3, c3) =>
        ???
      case _ => ???
    }

    (x2: Any) match {
      case (a3, b3, c3) =>
        ???
      case (a2, b2) =>
        assert(a2 == "a")
        assert(b2 == 2)
      case _ => ???
    }

    (x3: Any) match {
      case (a2, b2) =>
        ???
      case (a3, b3, c3) =>
        assert(a3 == "a")
        assert(b3 == 2)
        assert(c3 == "c")
    }

    (x3: Any) match {
      case (a3, b3, c3) =>
        assert(a3 == "a")
        assert(b3 == 2)
        assert(c3 == "c")
      case (a2, b2) =>
        ???
    }

    (x4: Any) match {
      case (a3, b3, c3) =>
        ???
      case (a4, b4, c4, d4) =>
        assert(a4 == "a")
        assert(b4 == 2)
        assert(c4 == "c")
        assert(d4 == 3)
    }

    (x4: Any) match {
    case (a4, b4, c4, d4) =>
      assert(a4 == "a")
      assert(b4 == 2)
      assert(c4 == "c")
      assert(d4 == 3)
    case (a3, b3, c3) =>
      ???
    }

    (x5: Any) match {
      case (a4, b4, c4, d4) =>
        ???
      case (a5, b5, c5, d5, e5) =>
        assert(a5 == "a")
        assert(b5 == 2)
        assert(c5 == "c")
        assert(d5 == 3)
        assert(e5 == "e")
    }

    (x5: Any) match {
      case (a5, b5, c5, d5, e5) =>
        assert(a5 == "a")
        assert(b5 == 2)
        assert(c5 == "c")
        assert(d5 == 3)
        assert(e5 == "e")
      case (a4, b4, c4, d4) =>
        ???
    }

    (x21: Any) match {
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21) =>
        assert(e1 == 1)
        assert(e21 == 21)
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20) =>
        ???
    }

    (x21: Any) match {
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20) =>
        ???
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21) =>
        assert(e1 == 1)
        assert(e21 == 21)
    }

    (x22: Any) match {
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22) =>
        assert(e1 == 1)
        assert(e22 == 22)
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21) =>
        ???
    }

    (x22: Any) match {
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21) =>
        ???
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22) =>
        assert(e1 == 1)
        assert(e22 == 22)
    }

    (x23: Any) match {
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23) =>
        assert(e1 == 1)
        assert(e23 == 23)
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22) =>
        ???
    }

    (x23: Any) match {
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22) =>
        ???
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23) =>
        assert(e1 == 1)
        assert(e23 == 23)
    }

    (x24: Any) match {
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24) =>
        assert(e1 == 1)
        assert(e24 == 24)
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23) =>
        ???
    }

    (x24: Any) match {
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23) =>
        ???
      case (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24) =>
        assert(e1 == 1)
        assert(e24 == 24)
    }
  }
}
