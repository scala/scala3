import scala.compiletime.summonAll

@main def Test: Unit =
  assert(summonAll[EmptyTuple] == EmptyTuple)
  assert(summonAll[(5, 5, 5)] == (5, 5, 5))
  assert(
    summonAll[(
      5, 5, 5, 5, 5,
      5, 5, 5, 5, 5,
      5, 5, 5, 5, 5,
      5, 5, 5, 5, 5,
      5, 5, 5, 5, 5,
    )] == (
      5, 5, 5, 5, 5,
      5, 5, 5, 5, 5,
      5, 5, 5, 5, 5,
      5, 5, 5, 5, 5,
      5, 5, 5, 5, 5,
    ))

given 5 = 5
