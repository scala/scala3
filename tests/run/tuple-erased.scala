object Test {
  def main(args: Array[String]): Unit = {
    val t02: Any = (1, 2)
    val t05: Any = (1, 2, 4, 5, 6)
    val t21: Any = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
    val t22: Any = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    val t23: Any = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
    val t24: Any = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

    assert(t02.asInstanceOf[Tuple] == t02)
    assert(t05.asInstanceOf[Tuple] == t05)
    assert(t21.asInstanceOf[Tuple] == t21)
    assert(t22.asInstanceOf[Tuple] == t22)
    assert(t23.asInstanceOf[Tuple] == t23)
    assert(t24.asInstanceOf[Tuple] == t24)

    assert(t02.isInstanceOf[Tuple])
    assert(t05.isInstanceOf[Tuple])
    assert(t21.isInstanceOf[Tuple])
    assert(t22.isInstanceOf[Tuple])
    assert(t23.isInstanceOf[Tuple])
    assert(t24.isInstanceOf[Tuple])

    assert(t02.asInstanceOf[Product] == t02)
    assert(t05.asInstanceOf[Product] == t05)
    assert(t21.asInstanceOf[Product] == t21)
    assert(t22.asInstanceOf[Product] == t22)
    assert(t23.asInstanceOf[Product] == t23)
    assert(t24.asInstanceOf[Product] == t24)

    assert(t02.isInstanceOf[Product])
    assert(t05.isInstanceOf[Product])
    assert(t21.isInstanceOf[Product])
    assert(t22.isInstanceOf[Product])
    assert(t23.isInstanceOf[Product])
    assert(t24.isInstanceOf[Product])

    assert(t02.asInstanceOf[*:[_, _]] == t02)
    assert(t05.asInstanceOf[*:[_, _]] == t05)
    assert(t21.asInstanceOf[*:[_, _]] == t21)
    assert(t22.asInstanceOf[*:[_, _]] == t22)
    assert(t23.asInstanceOf[*:[_, _]] == t23)
    assert(t24.asInstanceOf[*:[_, _]] == t24)

    assert(t02.isInstanceOf[*:[_, _]])
    assert(t05.isInstanceOf[*:[_, _]])
    assert(t21.isInstanceOf[*:[_, _]])
    assert(t22.isInstanceOf[*:[_, _]])
    assert(t23.isInstanceOf[*:[_, _]])
    assert(t24.isInstanceOf[*:[_, _]])

    val x = Tuple()
    assert(x.asInstanceOf[Tuple] == x)
    assert(x.asInstanceOf[Any] == x)

    val y: *:[Int, *:[String, EmptyTuple]] = (1, "s")
    y: Tuple
  }
}
