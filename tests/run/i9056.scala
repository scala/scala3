
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

  assert(!((): Any).isInstanceOf[_ *: _])
  assert((1, 2).isInstanceOf[_ *: _])
  assert((1, 2, 3).isInstanceOf[_ *: _])
  assert(!("": Any).isInstanceOf[_ *: _])
  assert(!(Some(""): Any).isInstanceOf[_ *: _])
