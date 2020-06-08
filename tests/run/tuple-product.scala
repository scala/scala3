@main def Test = {
  val a: Product = 1 *: Tuple()
  assert(a.productArity == 1)
  val b: Product = 1 *: 2 *: Tuple()
  assert(b.productArity == 2)
  val c: Product = 1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: 22 *: 23 *: 24 *: 25 *: Tuple()
  assert(c.productArity == 25)
  val d: NonEmptyTuple = (1, 2)
  val e: Product = d
  assert(e.productArity == 2)
}