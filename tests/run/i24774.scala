case class A() extends Exception

object Test:
  def main(args: Array[String]): Unit =
    try {
      throw A();
    } catch {
      case A() => Console.println("OK");
      case ex: Exception => Console.println("should not reach here: " + ex);
    }
