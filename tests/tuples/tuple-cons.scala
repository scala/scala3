import dotty._

object Test {
  def main(args: Array[String]) = {
    val t1 = (1, 2)

    val t2: TupleCons[Int, TupleCons[Int, TupleCons[Int, Unit]]] = TupleCons(0, t1)

    assert(t2._1 == 0)
    assert(t2._2 == 1)
    assert(t2._3 == 2)

    t2 match {
      case TupleCons(e1, TupleCons(e2, TupleCons(e3, ()))) =>
        assert(e1 == 0)
        assert(e2 == 1)
        assert(e3 == 2)
    }

    val t3: TupleCons[Int, TupleCons[Int, TupleCons[Int, TupleCons[Int, Unit]]]] = TupleCons(-1, t2)

    assert(t3._1 == -1)
    assert(t3._2 == 0)
    assert(t3._3 == 1)
    assert(t3._4 == 2)

    t3 match {
      case TupleCons(e1, TupleCons(e2, TupleCons(e3, TupleCons(e4, ())))) =>
        assert(e1 == -1)
        assert(e2 == 0)
        assert(e3 == 1)
        assert(e4 == 2)
    }

    val C = TupleCons
    type C[A, B <: Tuple] = TupleCons[A, B]
    type I = Int

    //       1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20
    val t20: C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,Unit]]]]]]]]]]]]]]]]]]]] = C(1,C(2,C(3,C(4,C(5,C(6,C(7,C(8,C(9,C(10,C(11,C(12,C(13,C(14,C(15,C(16,C(17,C(18,C(19,C(20,()))))))))))))))))))))

    t20 match {
      case C(e1, C(e2, C(e3, C(e4, C(e5, C(e6, C(e7, C(e8, C(e9, C(e10, C(e11, C(e12, C(e13, C(e14, C(e15, C(e16, C(e17, C(e18, C(e19, C(e20, ())))))))))))))))))))) =>
      assert(e1 == 1)
      assert(e2 == 2)
      assert(e3 == 3)
      assert(e4 == 4)
      assert(e5 == 5)
      assert(e6 == 6)
      assert(e7 == 7)
      assert(e8 == 8)
      assert(e9 == 9)
      assert(e10 == 10)
      assert(e11 == 11)
      assert(e12 == 12)
      assert(e13 == 13)
      assert(e14 == 14)
      assert(e15 == 15)
      assert(e16 == 16)
      assert(e17 == 17)
      assert(e18 == 18)
      assert(e19 == 19)
      assert(e20 == 20)
    }

    //       1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21
    val t21: C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,Unit]]]]]]]]]]]]]]]]]]]]] = C(21, t20)

    t21 match {
      case C(e21, C(e1, C(e2, C(e3, C(e4, C(e5, C(e6, C(e7, C(e8, C(e9, C(e10, C(e11, C(e12, C(e13, C(e14, C(e15, C(e16, C(e17, C(e18, C(e19, C(e20, ()))))))))))))))))))))) =>
      assert(e1 == 1)
      assert(e2 == 2)
      assert(e3 == 3)
      assert(e4 == 4)
      assert(e5 == 5)
      assert(e6 == 6)
      assert(e7 == 7)
      assert(e8 == 8)
      assert(e9 == 9)
      assert(e10 == 10)
      assert(e11 == 11)
      assert(e12 == 12)
      assert(e13 == 13)
      assert(e14 == 14)
      assert(e15 == 15)
      assert(e16 == 16)
      assert(e17 == 17)
      assert(e18 == 18)
      assert(e19 == 19)
      assert(e20 == 20)
      assert(e21 == 21)
    }

    //       1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22
    val t22: C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,Unit]]]]]]]]]]]]]]]]]]]]]] = C(22, t21)

    t22 match {
      case C(e22, C(e21, C(e1, C(e2, C(e3, C(e4, C(e5, C(e6, C(e7, C(e8, C(e9, C(e10, C(e11, C(e12, C(e13, C(e14, C(e15, C(e16, C(e17, C(e18, C(e19, C(e20, ())))))))))))))))))))))) =>
      assert(e1 == 1)
      assert(e2 == 2)
      assert(e3 == 3)
      assert(e4 == 4)
      assert(e5 == 5)
      assert(e6 == 6)
      assert(e7 == 7)
      assert(e8 == 8)
      assert(e9 == 9)
      assert(e10 == 10)
      assert(e11 == 11)
      assert(e12 == 12)
      assert(e13 == 13)
      assert(e14 == 14)
      assert(e15 == 15)
      assert(e16 == 16)
      assert(e17 == 17)
      assert(e18 == 18)
      assert(e19 == 19)
      assert(e20 == 20)
      assert(e21 == 21)
      assert(e22 == 22)
    }

    //       1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23
    val t23: C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,Unit]]]]]]]]]]]]]]]]]]]]]]] = C(23, t22)

    t23 match {
      case C(e23, C(e22, C(e21, C(e1, C(e2, C(e3, C(e4, C(e5, C(e6, C(e7, C(e8, C(e9, C(e10, C(e11, C(e12, C(e13, C(e14, C(e15, C(e16, C(e17, C(e18, C(e19, C(e20, ()))))))))))))))))))))))) =>
      assert(e1 == 1)
      assert(e2 == 2)
      assert(e3 == 3)
      assert(e4 == 4)
      assert(e5 == 5)
      assert(e6 == 6)
      assert(e7 == 7)
      assert(e8 == 8)
      assert(e9 == 9)
      assert(e10 == 10)
      assert(e11 == 11)
      assert(e12 == 12)
      assert(e13 == 13)
      assert(e14 == 14)
      assert(e15 == 15)
      assert(e16 == 16)
      assert(e17 == 17)
      assert(e18 == 18)
      assert(e19 == 19)
      assert(e20 == 20)
      assert(e21 == 21)
      assert(e22 == 22)
      assert(e23 == 23)
    }

    //       1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
    val t24: C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,Unit]]]]]]]]]]]]]]]]]]]]]]]] = C(24, t23)

    t24 match {
      case C(e24, C(e23, C(e22, C(e21, C(e1, C(e2, C(e3, C(e4, C(e5, C(e6, C(e7, C(e8, C(e9, C(e10, C(e11, C(e12, C(e13, C(e14, C(e15, C(e16, C(e17, C(e18, C(e19, C(e20, ())))))))))))))))))))))))) =>
      assert(e1 == 1)
      assert(e2 == 2)
      assert(e3 == 3)
      assert(e4 == 4)
      assert(e5 == 5)
      assert(e6 == 6)
      assert(e7 == 7)
      assert(e8 == 8)
      assert(e9 == 9)
      assert(e10 == 10)
      assert(e11 == 11)
      assert(e12 == 12)
      assert(e13 == 13)
      assert(e14 == 14)
      assert(e15 == 15)
      assert(e16 == 16)
      assert(e17 == 17)
      assert(e18 == 18)
      assert(e19 == 19)
      assert(e20 == 20)
      assert(e21 == 21)
      assert(e22 == 22)
      assert(e23 == 23)
      assert(e24 == 24)
    }
  }
}
