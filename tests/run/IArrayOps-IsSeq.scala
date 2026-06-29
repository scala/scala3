import scala.collection.generic.{IsIterable, IsSeq}

extension [Repr](repr: Repr)(using iter: IsIterable[Repr])
  def sumBy[B: Numeric](f: iter.A => B): B =
    // from https://docs.scala-lang.org/overviews/core/custom-collection-operations.html
    val coll = iter(repr)
    val it = coll.iterator
    var result = f(it.next())
    while it.hasNext do
      result = summon[Numeric[B]].plus(result, f(it.next()))
    result

def iarraySeqOps[Repr](repr: Repr)(using iter: IsIterable[Repr] { type A = Int; type C = IArray[Int] }): Unit =
  val coll = iter(repr)
  assert(coll.map(_ * 2).getClass() == classOf[scala.collection.immutable.ArraySeq.ofRef[?]])
  val evens: IArray[Int] = coll.filter(_ % 2 == 0)
  val buckets: Map[Boolean, IArray[Int]] = coll.groupBy(_ < 3)
  val smalls: IArray[Int] = buckets(true)
  assert(smalls.mkString(",") == "1,2")

@main def Test: Unit =
  val isSeq1: IsSeq[IArray[Int]] { type A = Int; type C = IArray[Int] } =
    IsSeq.iarrayIsSeq[Int]
  val isSeq2: IsSeq[IArray[Int]] { type A = Int; type C = IArray[Int] } =
    summon[IsSeq[IArray[Int]]]
  val isSeq3: IsIterable[IArray[Int]] { type A = Int; type C = IArray[Int] } =
    summon[IsIterable[IArray[Int]]]
  assert(isSeq1.getClass == isSeq2.getClass) // check that default implicit is same as explicit call
  assert(isSeq1.getClass == isSeq3.getClass) // check widening still works

  val iarr = IArray(1, 2, 3)
  iarraySeqOps(iarr)

  assert(iarr.sumBy(identity) == 6)
