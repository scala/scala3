import compiletime.constValueTuple

@main def Test =
  println(constValueTuple["foo" *: "bar" *: 10 *: 2.5 *: EmptyTuple])
