//> using options -Werror

import java.lang.reflect.*
import scala.annotation.tailrec

class Test:
  @tailrec private def unwrapThrowable(x: Throwable): Throwable = x match {
    case  _: InvocationTargetException |      // thrown by reflectively invoked method or constructor
          _: ExceptionInInitializerError |    // thrown when running a static initializer (e.g. a scala module constructor)
          _: UndeclaredThrowableException |   // invocation on a proxy instance if its invocation handler's `invoke` throws an exception
          _: ClassNotFoundException |         // no definition for a class instantiated by name
          _: NoClassDefFoundError             // the definition existed when the executing class was compiled, but can no longer be found
      if x.getCause != null =>
      unwrapThrowable(x.getCause)
    case _ => x
  }

  private def unwrapHandler[T](pf: PartialFunction[Throwable, T]): PartialFunction[Throwable, T] =
    pf.compose({ case ex => unwrapThrowable(ex) })

  def test =
    unwrapHandler({ case ex => throw ex })
