import compiletime.summonAll

@main def Test =
  given as Int = 10
  given as String = "foo"
  given as Double = 1.2
  println(summonAll[Int *: String *: Double *: EmptyTuple])
