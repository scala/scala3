case class A() extends Exception

@main def Test: Unit =
  try throw A()
  catch
    case A() => // OK
    case ex: Exception => throw RuntimeException("should not reach here: " + ex);
