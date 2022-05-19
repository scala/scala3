import scala.quoted.*
def foo(using Quotes): Unit =
  '{ def bar[T](): Unit = ${ summon[Type[T]]; ??? }; () }
