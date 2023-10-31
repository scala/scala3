
@main def Test =
  ((): Any).isInstanceOf[Tuple]
  assert((1, 2).isInstanceOf[Tuple])
  assert((1, 2, 3).isInstanceOf[Tuple])
  assert(!"".isInstanceOf[Tuple])
  assert(!Some("").isInstanceOf[Tuple])

  assert(!((): Any).isInstanceOf[NonEmptyTuple])
  assert((1, 2).isInstanceOf[NonEmptyTuple])
  assert((1, 2, 3).isInstanceOf[NonEmptyTuple])
  assert(!("": Any).isInstanceOf[NonEmptyTuple])
  assert(!(Some(""): Any).isInstanceOf[NonEmptyTuple])

  assert(!((): Any).isInstanceOf[? *: ?])
  assert((1, 2).isInstanceOf[? *: ?])
  assert((1, 2, 3).isInstanceOf[? *: ?])
  assert(!("": Any).isInstanceOf[? *: ?])
  assert(!(Some(""): Any).isInstanceOf[? *: ?])
