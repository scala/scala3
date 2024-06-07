import language.experimental.captureChecking
def Test =
  val x: Any = ???
  val y = x.asInstanceOf[Int => Int]
