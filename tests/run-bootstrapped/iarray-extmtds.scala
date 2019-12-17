
object Test extends App {
  def arrToString[T](arr: IArray[T]): Unit =
    arr.asInstanceOf[Array[T]].mkString("IArray(", ", ", ")")

  // This is used to check the correct result, as well as checking that the IArray was not mutated in place
  def assertNew[T, U](expr: => IArray[T], sources: IArray[U]*): Unit = {
    val result = expr

    sources.foreach(source =>
      assert(result.asInstanceOf[AnyRef] ne source.asInstanceOf[AnyRef], "IArray was mutated in place")
    )

    println(arrToString(result))
  }

  val arr1 = IArray[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val arr2 = IArray[Int](11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

  assertNew(arr1 ++ arr2, arr1, arr2)

  println(arr1.contains(1))
  println(arr1.contains(25))

  println(arr1.count(_ % 2 == 0))

  assertNew(arr1.drop(8), arr1)

  assertNew(arr1.dropRight(8), arr1)

  assertNew(arr1.dropWhile(_ < 8))

  println(arr1.exists(_ % 6 == 0))

  assertNew(arr1.filter(_ % 2 == 0), arr1)

  assertNew(arr1.filterNot(_ % 2 == 0), arr1)

  println(arr1.find(_ % 5 == 0))
}
