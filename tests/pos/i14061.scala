def main(args: Array[String]): Unit =
  val bar: PartialFunction[Throwable, Unit] =
    case e: IllegalArgumentException => e.printStackTrace
    case e: Throwable => e.printStackTrace
  try
    println("a")
  catch
    bar
  finally
    println("a")