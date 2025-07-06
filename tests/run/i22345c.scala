def makeSeq[T](args: T*): Seq[T] = args

@main def Test: Unit =
  val a: Array[(Int, String)] = makeSeq[Int *: String *: EmptyTuple]().toArray
