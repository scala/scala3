import scala.util.control.*

object O:
  def foo() = ???

  def m(): Unit =
    try
      foo()
    catch
      case _: Throwable => () // warn

    try
      foo()
    catch
      case _: Error => () // warn

    try
      foo()
    catch
      case _: AssertionError => () // warn

    try
      foo()
    catch
      case NonFatal(_) => () // ok

    try
      foo()
    catch
      case _: Exception => () // ok

    try
      foo()
    catch
      case _: IndexOutOfBoundsException => () // ok

