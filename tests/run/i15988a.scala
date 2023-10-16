import scala.compiletime.constValueTuple

@main def Test: Unit =
  assert(constValueTuple[EmptyTuple] == EmptyTuple)
  assert(constValueTuple[("foo", 5, 3.14, "bar", false)] == ("foo", 5, 3.14, "bar", false))
  assert(constValueTuple[(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)] == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
