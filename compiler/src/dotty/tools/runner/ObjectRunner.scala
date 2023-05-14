package dotty.tools
package runner

import java.net.URL
import scala.util.control.NonFatal
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.UndeclaredThrowableException
import java.util.concurrent.ExecutionException

/**
 * This is a copy implementation from scala/scala scala.tools.nsc.CommonRunner trait
 */
trait CommonRunner {
  /** Run a given object, specified by name, using a
   *  specified classpath and argument list.
   *
   *  @throws java.lang.ClassNotFoundException
   *  @throws java.lang.NoSuchMethodException
   *  @throws java.lang.reflect.InvocationTargetException
   */
  def run(urls: Seq[URL], objectName: String, arguments: Seq[String]): Unit = {
    import RichClassLoader._
    ScalaClassLoader.fromURLsParallelCapable(urls).run(objectName, arguments)
  }

  /** Catches any non-fatal exception thrown by run (in the case of InvocationTargetException,
   *  unwrapping it) and returns it in an Option.
   */
  def runAndCatch(urls: Seq[URL], objectName: String, arguments: Seq[String]): Option[Throwable] =
    try   { run(urls, objectName, arguments) ; None }
    catch { case NonFatal(e) => Some(rootCause(e)) }

  private def rootCause(x: Throwable): Throwable = x match {
    case  _: InvocationTargetException |
          _: ExceptionInInitializerError |
          _: UndeclaredThrowableException |
          _: ExecutionException
            if x.getCause != null =>
              rootCause(x.getCause.nn)
    case _ => x
  }
}

/** An object that runs another object specified by name.
 *
 *  @author  Lex Spoon
 */
object ObjectRunner extends CommonRunner
