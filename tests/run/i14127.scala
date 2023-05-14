import scala.deriving.Mirror

@main def Test =
  val mISB = summon[Mirror.Of[Int *: String *: Boolean *: EmptyTuple]]
  assert(mISB.fromProduct((1, "foo", true)) == (1, "foo", true))

  val mT22 = summon[Mirror.Of[(
    Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int
      *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int
      *: Int *: Int *: Int *: Int *: EmptyTuple)]]

  // tuple of 22 elements
  val t22 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  assert(mT22.fromProduct(t22) == t22)
