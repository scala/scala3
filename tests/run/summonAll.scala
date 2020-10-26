import compiletime.summonAll

@main def Test =
  given Int = 10
  given String = "foo"
  given Double = 1.2
  println(summonAll[Int *: String *: Double *: EmptyTuple])
