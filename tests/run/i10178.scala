@main def Test: Unit =
  for
    x <- Option(23)
    given Int = x
  do assert(summon[Int] == 23)
