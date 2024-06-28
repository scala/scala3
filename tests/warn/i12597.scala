//> using options  -deprecation

@main def Test =
  val a: IArray[Int] = IArray(2)
  val b: IArray[Any] = a
  val c = b.toArray // warn: deprecated
  c(0) = ""

