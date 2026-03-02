import language.experimental.captureChecking
def foo(): Unit =
  val r1: IArray[String] = ???
  val r2: String = IArray.head(r1)