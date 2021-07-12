case class TBox[A <: Tuple](v: A)
case class IBox[A <: Int](v: A)

@main def m =
    val t: TBox[EmptyTuple] = TBox(EmptyTuple)
    val tt: Tuple.Map[(EmptyTuple, EmptyTuple), TBox] = (TBox(EmptyTuple), TBox(EmptyTuple))

    val tt2: Tuple.Map[(1, 2), IBox] = (IBox(1), IBox(2))

    type F[X] = (X, X)
    val tt3: Tuple.FlatMap[(1, 2), F] = (1, 1, 2, 2)
