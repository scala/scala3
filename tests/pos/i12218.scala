import language.experimental.fewerBraces
@main def runTest(): Unit =
  val arr = Array(1,2,3)
  if
    arr.isEmpty
    || locally:
      val first = arr(0)
      first != 1
  then println("invalid arr")
  else println("valid arr")
