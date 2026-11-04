class A extends Exception
class B extends Exception

object X:
  def run =
    try
      throw new Exception()
    catch
      case _: (A | B) =>
        println("A | B")
      case _ =>
        println("default")