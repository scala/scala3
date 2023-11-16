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

  // Examples of arity and nesting arity
  // to find the thresholds and reproduce the behaviour of nsc
  def examples(): Unit =
    val max1 = List[Object]("1", "2", "3", "4", "5", "6", "7")                  // 7 cons w/ 7 string heads + nil
    val max2 = List[Object]("1", "2", "3", "4", "5", "6", List[Object]())       // 7 cons w/ 6 string heads + 1 nil head + nil
    val max3 = List[Object]("1", "2", "3", "4", "5", List[Object]("6"))
    val max4 = List[Object]("1", "2", "3", "4", List[Object]("5", "6"))

    val over1 = List[Object]("1", "2", "3", "4", "5", "6", "7", "8")            // wrap 8-sized array
    val over2 = List[Object]("1", "2", "3", "4", "5", "6", "7", List[Object]()) // wrap 8-sized array
    val over3 = List[Object]("1", "2", "3", "4", "5", "6", List[Object]("7"))   // wrap 1-sized array with 7
    val over4 = List[Object]("1", "2", "3", "4", "5", List[Object]("6", "7"))   // wrap 2

    val max5 =
      List[Object](
        List[Object](
          List[Object](
            List[Object](
              List[Object](
                List[Object](
                  List[Object](
                    List[Object](
      )))))))) // 7 cons + 1 nil

    val over5 =
      List[Object](
        List[Object](
          List[Object](
            List[Object](
              List[Object](
                List[Object](
                  List[Object](
                    List[Object]( List[Object]()
      )))))))) // 7 cons + 1-sized array wrapping nil

    val max6 =
      List[Object](                         //  ::(
        "1", "2", List[Object](             //    1, ::(2, ::(::(
          "3", "4", List[Object](           //      3, ::(4, ::(::(
            List[Object]()                  //        Nil, Nil
          )                                 //      ), Nil))
        )                                   //    ), Nil))
      )                                     //  )
      // 7 cons + 4 string heads + 4 nils for nested lists

    val max7 =
      List[Object](                         //  ::(
        "1", "2", List[Object](             //    1, ::(2, ::(::(
          "3", "4", List[Object](           //      3, ::(4, ::(::(
            "5"                             //        5, Nil
          )                                 //      ), Nil))
        )                                   //    ), Nil))
      )                                     //  )
      // 7 cons + 5 string heads + 3 nils for nested lists
