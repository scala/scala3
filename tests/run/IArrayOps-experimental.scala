//> using options -experimental

@main def Test: Unit =
  val isSeq1: scala.collection.generic.IsSeq[IArray[Int]] { type A = Int; type C = IArray[Int] } =
    scala.collection.generic.IsSeq.iarrayIsSeq[Int]
  val isSeq2: scala.collection.generic.IsSeq[IArray[Int]] { type A = Int; type C = IArray[Int] } =
    summon[scala.collection.generic.IsSeq[IArray[Int]]]
  assert(isSeq1.getClass eq isSeq2.getClass) // check that default implicit is same as explicit call

  val iarr = IArray(1, 2, 3)
  val seqOps: scala.collection.SeqOps[Int, Iterable, IArray[Int]] = isSeq1(iarr)
  println(seqOps.map(_ * 2).getClass()) // should be immutable.ArraySeq.ofRef
  val evens: IArray[Int] = seqOps.filter(_ % 2 == 0)
  val buckets: Map[Boolean, IArray[Int]] = seqOps.groupBy(_ < 3)
  val smalls: IArray[Int] = buckets(true)
  println(smalls.mkString(","))
