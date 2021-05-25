@main def Test =
  val a: IArray[Int] = IArray(2)
  val b: IArray[Any] = a
  val c = IArray.genericWrapArray(b).toArray
  c(0) = ""
