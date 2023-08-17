
object Test {
  def main(args: Array[String]) = {
    val t1: *:[Int, *:[Int, EmptyTuple]] = (1, 2)

    val t2: *:[Int, *:[Int, *:[Int, EmptyTuple]]] = 0 *: t1

    assert(t2.head == 0)
    assert(t2.tail.head == 1)
    assert(t2.tail.tail.head == 2)

    t2 match {
      case e1 *: e2 *: e3 *: Tuple() =>
        assert(e1 == 0)
        assert(e2 == 1)
        assert(e3 == 2)
    }

    val t3: *:[Int, *:[Int, *:[Int, *:[Int, EmptyTuple]]]] = -1 *: t2

    assert(t3.head == -1)
    assert(t3.tail.head == 0)
    assert(t3.tail.tail.head == 1)
    assert(t3.tail.tail.tail.head == 2)

    t3 match {
      case e1 *: e2 *: e3 *: e4 *: Tuple() =>
        assert(e1 == -1)
        assert(e2 == 0)
        assert(e3 == 1)
        assert(e4 == 2)
    }

    type I = Int

    //       1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20
    val t20: *:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,EmptyTuple]]]]]]]]]]]]]]]]]]]] = 1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: Tuple()

    t20 match {
      case e1 *: e2 *: e3 *: e4 *: e5 *: e6 *: e7 *: e8 *: e9 *: e10 *: e11 *: e12 *: e13 *: e14 *: e15 *: e16 *: e17 *: e18 *: e19 *: e20 *: Tuple() =>
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
    val t21: *:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,EmptyTuple]]]]]]]]]]]]]]]]]]]]] = 21 *: t20

    t21 match {
      case e21 *: e1 *: e2 *: e3 *: e4 *: e5 *: e6 *: e7 *: e8 *: e9 *: e10 *: e11 *: e12 *: e13 *: e14 *: e15 *: e16 *: e17 *: e18 *: e19 *: e20 *: Tuple() =>
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
    val t22: *:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,EmptyTuple]]]]]]]]]]]]]]]]]]]]]] = 22 *: t21

    t22 match {
      case e22 *: e21 *: e1 *: e2 *: e3 *: e4 *: e5 *: e6 *: e7 *: e8 *: e9 *: e10 *: e11 *: e12 *: e13 *: e14 *: e15 *: e16 *: e17 *: e18 *: e19 *: e20 *: Tuple() =>
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
    val t23: *:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,EmptyTuple]]]]]]]]]]]]]]]]]]]]]]] = 23 *: t22

    t23 match {
      case e23 *: e22 *: e21 *: e1 *: e2 *: e3 *: e4 *: e5 *: e6 *: e7 *: e8 *: e9 *: e10 *: e11 *: e12 *: e13 *: e14 *: e15 *: e16 *: e17 *: e18 *: e19 *: e20 *: Tuple() =>
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
    val t24: *:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,*:[I,EmptyTuple]]]]]]]]]]]]]]]]]]]]]]]] = 24 *: t23

    t24 match {
      case e24 *: e23 *: e22 *: e21 *: e1 *: e2 *: e3 *: e4 *: e5 *: e6 *: e7 *: e8 *: e9 *: e10 *: e11 *: e12 *: e13 *: e14 *: e15 *: e16 *: e17 *: e18 *: e19 *: e20 *: Tuple() =>
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
