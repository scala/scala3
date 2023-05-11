object Test1:
  def foo[T >: Any]: Unit = ()
  def bar[T]: Unit = foo[T & Any] // error

object Test2:
  def foo[T >: CharSequence]: Unit = ()
  val _ = foo[String]  // error
