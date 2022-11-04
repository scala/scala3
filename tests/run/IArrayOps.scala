@main def Test: Unit =
  val iarr1 = IArray(1, 2, 3)
  val iarr2 = IArray(5, 4, 5, 6)

  println(iarr1.toSeq)
  println(iarr1.contains(1))
  println(iarr1.contains(4))
  println(iarr1.updated(1, 9).toSeq)
  println(iarr1.updated(1, "a").toSeq)

  val arr = Array(0, 0, 0, 0)
  iarr1.copyToArray(arr)
  println(arr.toSeq)
  iarr1.copyToArray(arr, 1)
  println(arr.toSeq)
  iarr2.copyToArray(arr, 1, 2)
  println(arr.toSeq)

  println(arr.count(_ == 1))
  println(arr.count(_ <= 2))

  println(arr.drop(0).toSeq)
  println(arr.drop(1).toSeq)
  println(arr.drop(6).toSeq)

  println((iarr1 ++ iarr2).toSeq)
  println((iarr1 ++ iarr2.toSeq).toSeq)
  println((iarr1 :+ 7).toSeq)
  println((8 +: iarr1).toSeq)
  println((iarr1 :++ iarr2).toSeq)
  println((iarr2 :++ List(12, 13, 14)).toSeq)
  println((List(12, 13, 14) ++: iarr2).toSeq)

  val sb = StringBuilder()
  iarr1.addString(sb).append("\n")
  iarr1.addString(sb, "-").append("\n")
  iarr1.addString(sb, "(", ",", ")")
  println(sb)

  println(iarr1.appended(5).toSeq)
  println(iarr1.appendedAll(iarr2).toSeq)
  println(iarr1.appendedAll(List(1, 4)).toSeq)
  println(iarr1.prepended(5).toSeq)
  println(iarr1.prependedAll(List(5, 6)).toSeq)

  println(iarr1.collect { case x if x % 2 == 1 => 2 * x }.toSeq)
  println(iarr1.collectFirst { case x if x % 2 == 1 => 2 * x })

  println(iarr1.combinations(1).map(_.toSeq).toSeq)
  println(iarr1.combinations(2).map(_.toSeq).toSeq)
  println(iarr1.combinations(3).map(_.toSeq).toSeq)

  println(iarr1.concat(iarr2).toSeq)
  println(iarr1.concat(List(8, 9)).toSeq)
  println(iarr1.containsSlice(iarr2))
  println(iarr1.containsSlice(List(2, 3)))

  println(iarr1.corresponds(iarr2)(_ == _))
  println(iarr1.corresponds(List(1, 2, 3))(_ == _))
  println(iarr1.corresponds(List(2, 4, 6))(_ == 2 * _))

  println(iarr1.diff(List(1, 2)).toSeq)
  println(iarr1.diff(IArray(1, 2)).toSeq)

  println(iarr1.distinct.toSeq)
  println(iarr2.distinct.toSeq)
  println(iarr2.distinctBy(_ % 2 == 1).toSeq)

  println(iarr1.startsWith(IArray(2, 3)))
  println(iarr1.startsWith(List(2, 3)))
  println(iarr1.endsWith(IArray(2, 3)))
  println(iarr1.endsWith(List(2, 3)))
  println(iarr1.findLast(_ <= 1))
  println(iarr1.findLast(_ <= 2))

  println(iarr1.fold(0)(_ + _))
  println(iarr1.foldLeft(0)(_ + _))
  println(iarr1.foldRight(0)(_ + _))

  println(iarr1.reduce(_ + _))
  println(iarr1.reduceLeft(_ + _))
  println(iarr1.reduceRight(_ + _))

  println(iarr1.groupBy(identity).mapValues(_.toSeq).toMap)
  println(iarr1.groupBy(_ % 2).mapValues(_.toSeq).toMap)

  println(iarr1.groupMap(identity)(_.toString).mapValues(_.toSeq).toMap)
  println(iarr1.groupMap(_ % 2)(_.toString).mapValues(_.toSeq).toMap)

  println(iarr1.groupMapReduce(identity)(_.toString)(_ + _).mapValues(_.toSeq).toMap)
  println(iarr1.groupMapReduce(_ % 2)(_.toString)(_ + _).mapValues(_.toSeq).toMap)

  println(iarr1.grouped(2).map(_.toSeq).toList)

  println(iarr1.indexOfSlice(IArray(1, 2)))
  println(iarr1.indexOfSlice(List(2, 3)))
  println(iarr1.indexOfSlice(IArray(1, 2), 1))
  println(iarr1.indexOfSlice(List(2, 3), 1))
  println(iarr1.lastIndexOfSlice(IArray(1, 2)))
  println(iarr1.lastIndexOfSlice(List(2, 3)))
  println(iarr1.lastIndexOfSlice(IArray(1, 2), 1))
  println(iarr1.lastIndexOfSlice(List(2, 3), 1))

  println(iarr1.inits.map(_.toSeq).toList)
  println(iarr1.tails.map(_.toSeq).toList)
  println(iarr1.intersect(List(1, 2)).toSeq)
  println(iarr1.intersect(List(2, 3)).toSeq)
  println(iarr1.isTraversableAgain)
  println(iarr1.knownSize)
  println(iarr1.lazyZip(iarr2))
  println(iarr1.lazyZip(iarr2.toSeq).toList)
  println(iarr1.lengthCompare(3))
  println(iarr1.lengthCompare(1))
  println(iarr1.lengthCompare(4))

  println(iarr1.max)
  println(iarr1.maxOption)
  println(iarr1.maxBy(identity))
  println(iarr1.maxByOption(identity))
  println(iarr1.min)
  println(iarr1.minOption)
  println(iarr1.minBy(identity))
  println(iarr1.minByOption(identity))

  println(iarr1.mkString)
  println(iarr1.mkString("-"))
  println(iarr1.mkString("(", ",", ")"))

  println(iarr1.zip(iarr2.toSeq).toSeq)
  println(iarr1.zip(Iterator(5, 5, 5)).toSeq)
  println(iarr1.zipAll(IArray(5, 5), 3, 4).toSeq)
  println(iarr1.zipAll(List(5, 5), 3, 4).toSeq)

  println(IArray((1, 'a'), (2, 'b')).unzip._1.toSeq)
  println(IArray((1, 'a'), (2, 'b')).unzip._2.toSeq)
  println(IArray((1, 'a', "x"), (2, 'b', "y")).unzip3._1.toSeq)
  println(IArray((1, 'a', "x"), (2, 'b', "y")).unzip3._2.toSeq)
  println(IArray((1, 'a', "x"), (2, 'b', "y")).unzip3._3.toSeq)

  println(iarr1.padTo(6, 0).toSeq)
  println(iarr1.partitionMap[Int, Int](i => Left(i))._1.toSeq)
  println(iarr1.partitionMap[Int, Int](i => Left(i))._2.toSeq)
  println(iarr1.partitionMap[Int, Int](i => Right(i))._1.toSeq)
  println(iarr1.partitionMap[Int, Int](i => Right(i))._2.toSeq)
  println(iarr1.patch(1, List(6, 7, 8, 9), 2).toSeq)
  println(iarr1.permutations.map(_.toSeq).toList)

  println(iarr1.sum)
  println(iarr1.product)

  println(iarr1.iterator.toList)
  println(iarr1.reverseIterator.toList)

  println(iarr1.sameElements(iarr1))
  println(iarr1.sameElements(iarr2.toSeq))

  println(iarr1.segmentLength(_ < 2))
  println(iarr1.segmentLength(_ < 2, 1))

  println(iarr1.sizeCompare(iarr2))
  println(iarr1.sizeCompare(iarr2.toSeq))
  println(iarr1.sizeCompare(1))

  println(iarr1.sliding(2).map(_.toSeq).toList)
  println(iarr2.sliding(3, 2).map(_.toSeq).toList)

  println(iarr2.sortBy(i => -i).toSeq)

  println(iarr1.startsWith(Iterator(1, 2), 0))
  println(iarr1.startsWith(Iterator(2, 3), 1))

  println(iarr1.tapEach(x => println(x)).toSeq)

  println(iarr1.zipWithIndex.toSeq)

  def portableBufferToString[A](x: scala.collection.Iterable[A]): String =
    x match
      case x: scala.collection.mutable.Buffer[A] => x.mkString("Buffer(", ", ", ")")
      case _ => s"not a buffer $x"

  println(iarr1.to(List))
  println(iarr1.to(Vector))
  println(portableBufferToString(iarr1.toBuffer))
  println(iarr1.toIndexedSeq)
  println(iarr1.toIterable)
  println(iarr1.toList)
  println(iarr1.toSet)
  println(iarr1.toVector)
  println(iarr1.view)
  println(iarr1.zip(iarr2).toMap)
  println(iarr1.zip(iarr2.toSeq).toMap)

  println(iarr1.empty)
  println(iarr1.isTraversableAgain)
  println(iarr1.lengthIs < 2)
  println(iarr1.sizeIs < 2)

  iarr1.stepper

  println(IArray(List(1, 2), List(2, 3)).flatten.toSeq)

  println(iarr1.search(2))

  println(iarr1.withFilter(_ != 2).map(identity).toSeq)

  for
    x <- iarr1
    if x != 1
    y <- iarr2
    if y != 5
    if y != 65
  do
    println((x, y))

  println(
    (for
      x <- iarr1
      if x != 1
      y <- iarr2.toSeq
      if y != 5
      if y != 65
    yield
      (x, y)
  ).toSeq)

  println(IArray[IArray[Int]]().transpose.map(_.toSeq).toSeq)
  println(IArray(IArray(1, 2, 3), IArray(4, 5, 6)).transpose.map(_.toSeq).toSeq)

  println(IArray.equals(IArray("a"), IArray("b")))
  println(IArray.from(List("a", "b")).toSeq)

  val iab = IArray.newBuilder[Int]
  iab += 1
  iab += 2
  iab.addOne(4)
  iab.addAll(iarr1)
  iab.addAll(Array(9))
  iab.addAll(Seq(10))
  println(iab.result().toSeq)


  println(
    (for
      x <- List(1)
      y <- iarr1
      y <- iarr2
    yield
      (x, y)
  ).toSeq)

  def portableToString(x: Any): String =
    if x == () then "()" else x.toString // portable on Scala.js

  println(
    (for
      x1 <- List(1)
      x2 <- IArray(true)
      x3 <- IArray(1: Byte)
      x4 <- IArray(2: Short)
      x5 <- IArray(3)
      x6 <- IArray(4L)
      x7 <- IArray(5.5f)
      x8 <- IArray(6.5d)
      x9 <- IArray('a')
      x10 <- IArray("abc")
      x11 <- IArray("xyz": Any)
      x12 <- IArray(())
    yield
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, portableToString(x12))
  ).toSeq)

  println(
    (for
      x1 <- IArray(1)
      x2 <- IArray(true)
      x3 <- IArray(1: Byte)
      x4 <- IArray(2: Short)
      x5 <- IArray(3)
      x6 <- IArray(4L)
      x7 <- IArray(5.5f)
      x8 <- IArray(6.5d)
      x9 <- IArray('a')
      x10 <- IArray("abc")
      x11 <- IArray("xyz": Any)
      x12 <- IArray(())
    yield
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, portableToString(x12))
  ).toSeq)
