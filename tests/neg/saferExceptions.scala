object test:
  import language.experimental.saferExceptions
  import java.io.IOException

  class Failure extends Exception

  def bar(x: Int): Int throws Failure | IOException =
    x match
      case 1 => throw AssertionError()
      case 2 => throw Failure()               // ok
      case 3 => throw java.io.IOException()   // ok
      case 4 => throw Exception()             // error
      case 5 => throw Throwable()             // ok: Throwable is treated as unchecked
      case _ => 0

  def foo(x: Int): Int throws Exception = bar(x)
  def baz(x: Int): Int throws Failure = bar(x)  // error
