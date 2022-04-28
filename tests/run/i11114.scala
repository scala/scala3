@main def Test() =
  println((1, 1).getClass)
  println(Tuple2.apply(1, 1).getClass)
  println(new Tuple2(1, 1).getClass)
