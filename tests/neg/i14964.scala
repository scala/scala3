// nopos-error
// nopos-error
@main def Test: Unit =
  val xs = (1, 2).toList
  val a1 = xs.toArray
  println((1, 2).toList.toArray)
  val a2 = (1, 2).toList.toArray
