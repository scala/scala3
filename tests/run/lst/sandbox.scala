@main def sandbox =
  import lst.Lst
  import Lst.+:
  val xs = Lst(1, 2, 3)

  xs match
    case x +: ys =>
      assert(x == 1)
      assert(ys === Lst(2, 3))

  xs match
    case Lst(x, ys: _*) =>
      assert(x == 1)
      assert(ys == Lst(2, 3).toList)

  xs match
    case x +: _ =>
      assert(x == 1)

  xs match
    case Lst(x, _: _*) =>
      assert(x == 1)
