def f(x: Int): (Int, Int) = (1, x)

val result: Iterable[(Int, Int)] =
  for
    (k, v) <- Map(1 -> 1, 2 -> 1, 3 -> 1)
    x      = k + v
    (a, b) = f(x) // warn previously map would have changed result type
    (y, z) <- Map(42 -> 27)
  yield (a+y, b+z)

@main def Test() =
  assert(result.size == 1, s"Expected 1 only, got ${result.size}")
  assert(result.head == (43, 31), result.head)

/*
3.8

    val result: Iterable[Tuple2[Int, Int]] =
      Map.apply[Int, Int](
        [ArrowAssoc[Int](1).->[Int](1),ArrowAssoc[Int](2).->[Int](1),
          ArrowAssoc[Int](3).->[Int](1) : (Int, Int)]*
      ).map[Int, Int]((x$1: (Int, Int)) =>
        x$1:(Int, Int) @RuntimeChecked match
          {
            case Tuple2.unapply[Int, Int](k @ _, v @ _) =>
              val x: Int = k + v
              val $2$: ((Int, Int), Int, Int) =
                f(x):(Int, Int) @RuntimeChecked match
                  {
                    case $1$ @ Tuple2.unapply[Int, Int](a @ _, b @ _) =>
                      Tuple3.apply[(Int, Int), Int, Int]($1$, a, b)
                  }
              val $1$: (Int, Int) = $2$._1
              val a: Int = $2$._2
              val b: Int = $2$._3
              Tuple2.apply[Int, Int](a, b)
          }
      )
3.7
    val result: Iterable[Tuple2[Int, Int]] =
      Map.apply[Int, Int](
        [ArrowAssoc[Int](1).->[Int](1),ArrowAssoc[Int](2).->[Int](1),
          ArrowAssoc[Int](3).->[Int](1) : (Int, Int)]*
      ).map[((Int, Int), Int, (Int, Int))]((x$1: (Int, Int)) =>
        x$1:(Int, Int) @unchecked match
          {
            case $1$ @ Tuple2.unapply[Int, Int](k @ _, v @ _) =>
              val x: Int = k + v
              val $3$: ((Int, Int), Int, Int) =
                f(x):(Int, Int) @unchecked match
                  {
                    case $2$ @ Tuple2.unapply[Int, Int](a @ _, b @ _) =>
                      Tuple3.apply[(Int, Int), Int, Int]($2$, a, b)
                  }
              val $2$: (Int, Int) = $3$._1
              val a: Int = $3$._2
              val b: Int = $3$._3
              Tuple3.apply[(Int, Int), Int, (Int, Int)]($1$, x, $2$)
          }
      ).map[(Int, Int)]((x$1: ((Int, Int), Int, (Int, Int))) =>
        x$1:(x$1 : ((Int, Int), Int, (Int, Int))) @unchecked match
          {
            case
              Tuple3.unapply[(Int, Int), Int, (Int, Int)](
                Tuple2.unapply[Int, Int](k @ _, v @ _), x @ _,
                Tuple2.unapply[Int, Int](a @ _, b @ _))
             => Tuple2.apply[Int, Int](a, b)
          }
      )
*/
