package dotty.tools
package runner

import java.lang.invoke.{MethodHandles, MethodType}

import scala.language.implicitConversions
import java.lang.{ClassLoader => JClassLoader}
import java.lang.reflect.Modifier
import java.net.{URLClassLoader => JURLClassLoader}
import java.net.URL

import scala.annotation.tailrec
import scala.util.control.Exception.catching
import scala.reflect.{ClassTag, classTag}
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.UndeclaredThrowableException
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.io.AbstractFile
import dotty.tools.io.Streamable
import scala.annotation.internal.sharable

trait HasClassPath {
  def classPathURLs: Seq[URL]
}

final class RichClassLoader(private val self: JClassLoader) extends AnyVal {
  /** Executing an action with this classloader as context classloader */
  def asContext[T](action: => T): T = {
    val saved = Thread.currentThread.getContextClassLoader
    try { ScalaClassLoader.setContext(self) ; action }
    finally ScalaClassLoader.setContext(saved)
  }

  /** Load and link a class with this classloader */
  def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = false)
  /** Load, link and initialize a class with this classloader */
  def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = true)

  private def tryClass[T <: AnyRef](path: String, initialize: Boolean): Option[Class[T]] =
    catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
      Class.forName(path, initialize, self).asInstanceOf[Class[T]]

  /** Create an instance of a class with this classloader */
  def create(path: String): AnyRef =
    tryToInitializeClass[AnyRef](path).map(_.getConstructor().newInstance()).orNull

  /** Create an instance with ctor args, or invoke errorFn before throwing. */
  def create[T <: AnyRef : ClassTag](path: String, errorFn: String => Unit)(args: AnyRef*): T = {
    def fail(msg: String) = error(msg, new IllegalArgumentException(msg))
    def error(msg: String, e: Throwable) = { errorFn(msg) ; throw e }
    try {
      val clazz = Class.forName(path, /*initialize =*/ true, /*loader =*/ self)
      if (classTag[T].runtimeClass isAssignableFrom clazz) {
        val ctor = {
          val maybes = clazz.getConstructors filter (c => c.getParameterCount == args.size &&
            (c.getParameterTypes zip args).forall { case (k, a) => k isAssignableFrom a.getClass })
          if (maybes.size == 1) maybes.head
          else fail(s"Constructor must accept arg list (${args map (_.getClass.getName) mkString ", "}): ${path}")
        }
        (ctor.newInstance(args: _*)).asInstanceOf[T]
      } else {
        errorFn(s"""Loader for ${classTag[T]}:   [${show(classTag[T].runtimeClass.getClassLoader)}]
                    |Loader for ${clazz.getName}: [${show(clazz.getClassLoader)}]""".stripMargin)
        fail(s"Not a ${classTag[T]}: ${path}")
      }
    } catch {
      case e: ClassNotFoundException =>
        error(s"Class not found: ${path}", e)
      case e @ (_: LinkageError | _: ReflectiveOperationException) =>
        error(s"Unable to create instance: ${path}: ${e.toString}", e)
    }
  }

  /** The actual bytes for a class file, or an empty array if it can't be found. */
  def classBytes(className: String): Array[Byte] = classAsStream(className) match {
    case null   => Array()
    case stream => Streamable.bytes(stream)
  }

  /** An InputStream representing the given class name, or null if not found. */
  def classAsStream(className: String) = self.getResourceAsStream {
    if (className endsWith ".class") className
    else s"${className.replace('.', '/')}.class"  // classNameToPath
  }

  /** Run the main method of a class to be loaded by this classloader */
  def run(objectName: String, arguments: Seq[String]): Unit = {
    val clsToRun = tryToInitializeClass(objectName) getOrElse (
      throw new ClassNotFoundException(objectName)
    )
    val method = clsToRun.getMethod("main", classOf[Array[String]])
    if (!Modifier.isStatic(method.getModifiers))
      throw new NoSuchMethodException(objectName + ".main is not static")

    try asContext(method.invoke(null, Array(arguments.toArray: AnyRef): _*)) // !!! : AnyRef shouldn't be necessary
    catch unwrapHandler({ case ex => throw ex })
  }

  @tailrec
  def unwrapThrowable(x: Throwable): Throwable = x match {
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
  def unwrapHandler[T](pf: PartialFunction[Throwable, T]): PartialFunction[Throwable, T] =
    pf.compose({ case ex => unwrapThrowable(ex) })

  def show(cl: ClassLoader): String = {
    import scala.reflect.Selectable.reflectiveSelectable

    @tailrec
    def isAbstractFileClassLoader(clazz: Class[_]): Boolean = {
      if (clazz == null) return false
      if (clazz == classOf[AbstractFileClassLoader]) return true
      isAbstractFileClassLoader(clazz.getSuperclass)
    }
    def inferClasspath(cl: ClassLoader): String = cl match {
      case cl: java.net.URLClassLoader if cl.getURLs != null =>
        (cl.getURLs mkString ",")
      case cl if cl != null && isAbstractFileClassLoader(cl.getClass) =>
        cl.asInstanceOf[{val root: AbstractFile}].root.canonicalPath
      case null =>
        val loadBootCp = (flavor: String) => scala.util.Properties.propOrNone(flavor + ".boot.class.path")
        loadBootCp("sun") orElse loadBootCp("java") getOrElse "<unknown>"
      case _ =>
        "<unknown>"
    }
    cl match {
      case null => s"primordial classloader with boot classpath [${inferClasspath(cl)}]"
      case _    => s"$cl of type ${cl.getClass} with classpath [${inferClasspath(cl)}] and parent being ${show(cl.getParent)}"
    }
  }
}

object RichClassLoader {
  implicit def wrapClassLoader(loader: ClassLoader): RichClassLoader = new RichClassLoader(loader)
}

/** A wrapper around java.lang.ClassLoader to lower the annoyance
 *  of java reflection.
 */
trait ScalaClassLoader extends JClassLoader {
  private def wrap = new RichClassLoader(this)
  /** Executing an action with this classloader as context classloader */
  def asContext[T](action: => T): T = wrap.asContext(action)

  /** Load and link a class with this classloader */
  def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = wrap.tryToLoadClass[T](path)
  /** Load, link and initialize a class with this classloader */
  def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = wrap.tryToInitializeClass(path)

  /** Create an instance of a class with this classloader */
  def create(path: String): AnyRef = wrap.create(path)

  /** Create an instance with ctor args, or invoke errorFn before throwing. */
  def create[T <: AnyRef : ClassTag](path: String, errorFn: String => Unit)(args: AnyRef*): T =
    wrap.create[T](path, errorFn)(args: _*)

  /** The actual bytes for a class file, or an empty array if it can't be found. */
  def classBytes(className: String): Array[Byte] = wrap.classBytes(className)

  /** An InputStream representing the given class name, or null if not found. */
  def classAsStream(className: String) = wrap.classAsStream(className)

  /** Run the main method of a class to be loaded by this classloader */
  def run(objectName: String, arguments: Seq[String]): Unit = wrap.run(objectName, arguments)
}


/** Methods for obtaining various classloaders.
 *      appLoader: the application classloader.  (Also called the java system classloader.)
 *      extLoader: the extension classloader.
 *     bootLoader: the boot classloader.
 *  contextLoader: the context classloader.
 */
object ScalaClassLoader {
  /** Returns loaders which are already ScalaClassLoaders unaltered,
   *  and translates java.net.URLClassLoaders into scala URLClassLoaders.
   *  Otherwise creates a new wrapper.
   */
  implicit def apply(cl: JClassLoader): ScalaClassLoader = cl match {
    case cl: ScalaClassLoader => cl
    case cl: JURLClassLoader  => new URLClassLoader(cl.getURLs.toSeq, cl.getParent)
    case _                    => new JClassLoader(cl) with ScalaClassLoader
  }
  def contextLoader = apply(Thread.currentThread.getContextClassLoader)
  def appLoader     = apply(JClassLoader.getSystemClassLoader)
  def setContext(cl: JClassLoader) = Thread.currentThread.setContextClassLoader(cl)

  class URLClassLoader(urls: Seq[URL], parent: JClassLoader)
      extends JURLClassLoader(urls.toArray, parent)
         with ScalaClassLoader
         with HasClassPath {
    private[this] var classloaderURLs: Seq[URL] = urls
    def classPathURLs: Seq[URL] = classloaderURLs

    /** Override to widen to public */
    override def addURL(url: URL) = {
      classloaderURLs :+= url
      super.addURL(url)
    }
    override def close(): Unit = {
      super.close()
      classloaderURLs = null
    }
  }

  def fromURLs(urls: Seq[URL], parent: ClassLoader = null): URLClassLoader = {
    new URLClassLoader(urls, if (parent == null) bootClassLoader else parent)
  }

  def fromURLsParallelCapable(urls: Seq[URL], parent: ClassLoader = null): JURLClassLoader = {
    new JURLClassLoader(urls.toArray, if (parent == null) bootClassLoader else parent)
  }

  /** True if supplied class exists in supplied path */
  def classExists(urls: Seq[URL], name: String): Boolean =
    (fromURLs(urls) tryToLoadClass name).isDefined

  /** Finding what jar a clazz or instance came from */
  def originOfClass(x: Class[_]): Option[URL] =
    Option(x.getProtectionDomain.getCodeSource) flatMap (x => Option(x.getLocation))

  @sharable private[this] val bootClassLoader: ClassLoader = {
    if (!scala.util.Properties.isJavaAtLeast("9")) null
    else {
      try {
        MethodHandles.lookup().findStatic(classOf[ClassLoader], "getPlatformClassLoader", MethodType.methodType(classOf[ClassLoader])).invoke().asInstanceOf[ClassLoader]
      } catch {
        case _: Throwable =>
          null
      }
    }
  }
}
