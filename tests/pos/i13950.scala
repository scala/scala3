def example =
  (1,2).map[[_] =>> Int]([C] => (x1: C) => x1 match {
    case x2: ([V] => () => Int) =>
      x2[Int]()
  })