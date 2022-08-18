@main def Test =
  val tup2: Any = (1, 2)
  val x1: (Int, Int) = tup2.asInstanceOf[(Int *: Int *: EmptyTuple) & (Int, Int)]
  val x2: (Int, Int) = tup2.asInstanceOf[(Int *: Int *: Tuple) & (Int, Int)]
  val x3: (Int, Int) = tup2.asInstanceOf[(Int *: Int *: EmptyTuple) | (Int, Int)]
  val x4: Tuple = tup2.asInstanceOf[(Int *: Int *: Tuple) | (Int, Int)]

  val tup3: Any = (1, 2, 3)
  val x5: (Int, Int, Int) = tup3.asInstanceOf[Int *: ((Int *: Int *: EmptyTuple) & (Int, Int))]
  val x6: (Int, Int, Int) = tup3.asInstanceOf[Int *: ((Int *: Int *: Tuple) & (Int, Int))]
  val x7: (Int, Int, Int) = tup3.asInstanceOf[Int *: ((Int *: Int *: EmptyTuple) | (Int, Int))]
  val x8: Tuple = tup3.asInstanceOf[Int *: ((Int *: Int *: Tuple) | (Int, Int))]
