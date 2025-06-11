
def getInt(): Int = 1

@main def Test =
  val x: {v: Int with v == 1} = getInt().runtimeChecked

  assertThrows[IllegalArgumentException]:
    val v1: {v: Int with v == 2} = x.runtimeChecked

def assertThrows[T <: Throwable](block: => Unit): Unit =
  try
    block
  catch
    case e: Throwable if e.isInstanceOf[T] =>
      return
    case _ =>
      throw new AssertionError("Unexpected exception")
  throw new AssertionError("Expected exception not thrown")
