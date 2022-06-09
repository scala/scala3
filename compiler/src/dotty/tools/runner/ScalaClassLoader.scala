package dotty.tools
package runner

import scala.language.unsafeNulls

import java.lang.ClassLoader
import java.lang.invoke.{MethodHandles, MethodType}
import java.lang.reflect.Modifier
import java.net.{ URL, URLClassLoader }
import java.lang.reflect.{ InvocationTargetException, UndeclaredThrowableException }

import scala.annotation.internal.sharable
import scala.annotation.tailrec
import scala.util.control.Exception.catching

final class RichClassLoader(private val self: ClassLoader) extends AnyVal {
  /** Execute an action with this classloader as context classloader. */
  private def asContext[T](action: => T): T = ScalaClassLoader.asContext(self)(action)

  /** Load and link a class with this classloader */
  def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = false)

  /** Load, link and initialize a class with this classloader */
  def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = true)

  private def tryClass[T <: AnyRef](path: String, initialize: Boolean): Option[Class[T]] =
    catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
      Class.forName(path, initialize, self).asInstanceOf[Class[T]]

  /** Run the main method of a class to be loaded by this classloader */
  def run(objectName: String, arguments: Seq[String]): Unit = {
    val clsToRun = tryToInitializeClass(objectName).getOrElse(throw new ClassNotFoundException(objectName))
    val method = clsToRun.getMethod("main", classOf[Array[String]])
    if !Modifier.isStatic(method.getModifiers) then
      throw new NoSuchMethodException(s"$objectName.main is not static")
    try asContext(method.invoke(null, Array(arguments.toArray: AnyRef): _*))
    catch unwrapHandler({ case ex => throw ex })
  }

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

  // Transforms an exception handler into one which will only receive the unwrapped
  // exceptions (for the values of wrap covered in unwrapThrowable.)
  private def unwrapHandler[T](pf: PartialFunction[Throwable, T]): PartialFunction[Throwable, T] =
    pf.compose({ case ex => unwrapThrowable(ex) })
}

object RichClassLoader {
  implicit def wrapClassLoader(loader: ClassLoader): RichClassLoader = new RichClassLoader(loader)
}

object ScalaClassLoader {
  def setContext(cl: ClassLoader) = Thread.currentThread.setContextClassLoader(cl)

  def fromURLsParallelCapable(urls: Seq[URL], parent: ClassLoader | Null = null): URLClassLoader =
    new URLClassLoader(urls.toArray, if parent == null then bootClassLoader else parent)

  @sharable private[this] val bootClassLoader: ClassLoader =
    if scala.util.Properties.isJavaAtLeast("9") then
      try
        ClassLoader.getSystemClassLoader.getParent 
      catch case _: Throwable => null
    else null

  extension (classLoader: ClassLoader)
    /** Execute an action with this classloader as context classloader. */
    def asContext[T](action: => T): T =
      val saved = Thread.currentThread.getContextClassLoader
      try
        setContext(classLoader)
        action
      finally setContext(saved)
}
