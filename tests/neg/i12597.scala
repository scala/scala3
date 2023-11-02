//> using options -Xfatal-warnings -deprecation

@main def Test =
  val a: IArray[Int] = IArray(2)
  val b: IArray[Any] = a
  val c = b.toArray // warn: deprecated
  c(0) = ""

// nopos-error: No warnings can be incurred under -Werror.
