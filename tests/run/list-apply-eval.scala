object Test:

  var counter = 0

  def next =
    counter += 1
    counter.toString

  def main(args: Array[String]): Unit =
    //List.apply is subject to an optimisation in cleanup
    //ensure that the arguments are evaluated in the currect order
    // Rewritten to:
    //      val myList: List = new collection.immutable.::(Test.this.next(), new collection.immutable.::(Test.this.next(), new collection.immutable.::(Test.this.next(), scala.collection.immutable.Nil)));
    val myList = List(next, next, next)
    assert(myList == List("1", "2", "3"), myList)

    val mySeq = Seq(next, next, next)
    assert(mySeq == Seq("4", "5", "6"), mySeq)

    val emptyList = List[Int]()
    assert(emptyList == Nil)

    // just assert it doesn't throw CCE to List
    val queue = scala.collection.mutable.Queue[String]()

  // test for the cast instruction described in checkApplyAvoidsIntermediateArray
  def lub(b: Boolean): List[(String, String)] =
    if b then List(("foo", "bar")) else Nil

  // from minimising CI failure in oslib
  // again, the lub of :: and Nil is Product, which breaks ++ (which requires IterableOnce)
  def lub2(b: Boolean): Unit =
    Seq(1) ++ (if (b) Seq(2) else Nil)
