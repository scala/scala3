package dotty.tools.repl

import java.lang.ClassLoader
import java.lang.invoke.{MethodHandles, MethodType}
import java.lang.reflect.Modifier
import java.net.{ URL, URLClassLoader }
import java.lang.reflect.{ InvocationTargetException, UndeclaredThrowableException }

import scala.annotation.internal.sharable
import scala.annotation.tailrec
import scala.util.control.Exception.catching

object ScalaClassLoader {
  def setContext(cl: ClassLoader) = Thread.currentThread.setContextClassLoader(cl)

  def fromURLsParallelCapable(urls: Seq[URL], parent: ClassLoader | Null = null): URLClassLoader =
    new URLClassLoader(urls.toArray, if parent == null then bootClassLoader else parent)

  @sharable private val bootClassLoader: ClassLoader | Null =
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
