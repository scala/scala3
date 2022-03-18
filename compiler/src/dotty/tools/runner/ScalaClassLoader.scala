package dotty.tools
package runner

import scala.language.unsafeNulls

import java.lang.ClassLoader
import java.lang.invoke.{MethodHandles, MethodType}
import java.lang.reflect.{Modifier, InvocationTargetException, UndeclaredThrowableException}
import java.net.{URL, URLClassLoader}

import scala.annotation.internal.sharable
import scala.annotation.tailrec
import scala.util.control.Exception.catching

object ClassLoaderOps:
  private def setContext(cl: ClassLoader) = Thread.currentThread.setContextClassLoader(cl)

  extension (self: ClassLoader)
    /** Execute an action with this classloader as context classloader. */
    def asContext[T](action: => T): T =
      val saved = Thread.currentThread.getContextClassLoader
      try
        setContext(self)
        action
      finally setContext(saved)

    /** Load and link a class with this classloader */
    def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = false)

    /** Load, link and initialize a class with this classloader */
    def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = true)

    private def tryClass[T <: AnyRef](path: String, initialize: Boolean): Option[Class[T]] =
      catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
        Class.forName(path, initialize, self).asInstanceOf[Class[T]]

    /** The actual bytes for a class file, or an empty array if it can't be found. */
    def classBytes(className: String): Array[Byte] = classAsStream(className) match
      case null   => Array()
      case stream => dotty.tools.io.Streamable.bytes(stream)

    private inline def classAsStream(className: String) = self.getResourceAsStream {
      if className.endsWith(".class") then className
      else s"${className.replace('.', '/')}.class"  // classNameToPath
    }

    /** Run the main method of a class to be loaded by this classloader */
    def runMain(objectName: String, arguments: Seq[String]): Unit =
      val clsToRun = tryToInitializeClass(objectName).getOrElse(throw ClassNotFoundException(objectName))
      val method = clsToRun.getMethod("main", classOf[Array[String]])
      if !Modifier.isStatic(method.getModifiers) then
        throw NoSuchMethodException(s"$objectName.main is not static")
      try asContext(method.invoke(null, Array(arguments.toArray: AnyRef)*))
      catch unwrapHandler({ case ex => throw ex })

    @tailrec private def unwrapThrowable(x: Throwable): Throwable = x match
      case  _: InvocationTargetException |      // thrown by reflectively invoked method or constructor
            _: ExceptionInInitializerError |    // thrown when running a static initializer (e.g. a scala module constructor)
            _: UndeclaredThrowableException |   // invocation on a proxy instance if its invocation handler's `invoke` throws an exception
            _: ClassNotFoundException |         // no definition for a class instantiated by name
            _: NoClassDefFoundError             // the definition existed when the executing class was compiled, but can no longer be found
              if x.getCause != null =>
                unwrapThrowable(x.getCause)
      case _ => x

    // Transforms an exception handler into one which will only receive the unwrapped
    // exceptions (for the values of wrap covered in unwrapThrowable.)
    private def unwrapHandler[T](pf: PartialFunction[Throwable, T]): PartialFunction[Throwable, T] =
      pf.compose({ case ex => unwrapThrowable(ex) })
end ClassLoaderOps

object ScalaClassLoader:
  def fromURLsParallelCapable(urls: Seq[URL], parent: ClassLoader | Null = null): URLClassLoader =
    new URLClassLoader(urls.toArray, if parent == null then bootClassLoader else parent)

  @sharable private[this] val bootClassLoader: ClassLoader =
    if scala.util.Properties.isJavaAtLeast("9") then
      try
        MethodHandles.lookup().findStatic(classOf[ClassLoader], "getPlatformClassLoader", MethodType.methodType(classOf[ClassLoader])).invoke().asInstanceOf[ClassLoader]
      catch case _: Throwable => null
    else null
end ScalaClassLoader
