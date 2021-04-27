
object Test extends App {
  def printIArray[T](arr: IArray[T]): Unit =
    println(arr.asInstanceOf[Array[T]].mkString("IArray(", ",", ")"))

  // This is used to check the correct result, as well as checking that the IArray was not mutated in place
  def assertDifferent[T, U](expr: IArray[T], sources: IArray[U]*): Unit = {
    sources.foreach(source =>
      assert(expr.asInstanceOf[AnyRef] ne source.asInstanceOf[AnyRef], "IArray was mutated in place")
    )

    printIArray(expr)
  }

  val arr1 = IArray[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val arr2 = IArray[Int](11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

  assertDifferent(arr1 ++ arr2, arr1, arr2)

  println(arr1.contains(7))

  println(arr1.count(_ % 2 == 0))

  assertDifferent(arr1.drop(8), arr1)

  assertDifferent(arr1.dropRight(8), arr1)

  assertDifferent(arr1.dropWhile(_ < 8), arr1)

  println(arr1.exists(_ % 6 == 0))

  assertDifferent(arr1.filter(_ % 2 == 0), arr1)

  assertDifferent(arr1.filterNot(_ % 2 == 0), arr1)

  println(arr1.find(_ % 5 == 0))

  assertDifferent(arr1.flatMap(x => List(x, x)), arr1)

  val twoDArr = IArray(List(1, 2), List(3, 4))
  assertDifferent(twoDArr.flatten[Int], twoDArr)

  println(arr1.fold(0)(_ + _))

  println(arr1.foldLeft("")((acc, x) => acc + x.toString))

  println(arr1.foldRight("")((x, acc) => acc + x.toString))

  println(arr1.forall(_ > 5))

  arr1.foreach(x => println(x))

  println(arr2.head)

  println(arr1.headOption)

  println(arr1.indexOf(5, 7))

  println(arr1.indexWhere(_ > 3, 1))

  println(arr2.indices.mkString(","))

  assertDifferent(arr1.init, arr1)

  println(arr1.isEmpty)

  println(arr1.iterator.take(3).mkString(","))

  println(arr1.last)

  println(arr2.lastOption)

  println(arr2.lastIndexOf(17))

  println(arr1.lastIndexWhere(_ < 5))

  assertDifferent(arr1.map(_ + 10), arr1)

  println(arr1.nonEmpty)

  val (even, odd) = arr1.partition(_ % 2 == 0)
  assertDifferent(even, arr1)
  assertDifferent(odd, arr1)

  assertDifferent(arr1.reverse, arr1)

  assertDifferent(arr1.scan(0)(_ + _), arr1)

  assertDifferent(arr1.scanLeft("")((acc, x) => acc + x.toString), arr1)

  assertDifferent(arr1.scanRight("")((x, acc) => acc + x.toString), arr1)

  println(arr2.size)

  assertDifferent(arr1.slice(5,7), arr1)

  assertDifferent(arr1.sortBy(- _), arr1)

  assertDifferent(arr1.sortWith((x, y) => x.toString.length > y.toString.length || x < y), arr1)

  assertDifferent(arr1.sorted, arr1)

  val (smaller, greater) = arr1.span(_ < 5)
  assertDifferent(smaller, arr1)
  assertDifferent(greater, arr1)

  val (first, last) = arr1.splitAt(7)
  assertDifferent(first, arr1)
  assertDifferent(last, arr1)

  println(arr1.startsWith(IArray(1,2,3,4,5,6,42)))

  assertDifferent(arr1.tail, arr1)

  assertDifferent(arr1.take(3), arr1)

  assertDifferent(arr1.takeRight(4), arr1)

  assertDifferent(arr1.takeWhile(_ < 3), arr1)

  val tupArr = IArray[(Int, String)]((1, "1"), (2, "2"), (3, "3"))
  val (ints, strings) = tupArr.unzip
  assertDifferent(ints, tupArr)
  assertDifferent(strings, tupArr)

  assertDifferent(arr1.zip(arr2), arr1, arr2)
}
