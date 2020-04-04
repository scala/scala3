object Test {
  class EThrow extends Exception
  class ECatch extends Exception

  def ok(): Unit =
    try throw new EThrow
    catch { case _: ECatch => }

  def notOk(): Unit = {
    val pf: PartialFunction[Throwable, Unit] = {
      case e: ECatch =>
    }
    try throw new EThrow
    catch pf
  }

  def test(f: => Unit): Unit =
    try f catch {
      case _: EThrow    => println("OK")
      // case e: Throwable => println("! expect an EThrow but got " + e.getClass)
    }

  def main(args: Array[String]): Unit = {
    test { ok() }
    test { notOk() }
  }
}