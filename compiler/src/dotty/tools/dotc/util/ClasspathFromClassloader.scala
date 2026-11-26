package dotty.tools.dotc.util

import java.net.URLClassLoader
import java.nio.file.Paths

import dotty.tools.io.AbstractFileClassLoader

object ClasspathFromClassloader {

  /** Is `cl` an `AbstractFileClassLoader`, possibly loaded by another classloader?
   *  A type test is not enough in the REPL, where this class is loaded twice.
   */
  private def isAbstractFileClassLoader(cl: ClassLoader): Boolean =
    val name = classOf[AbstractFileClassLoader].getName
    var c: Class[?] | Null = cl.getClass
    while c != null && c.getName != name do c = c.getSuperclass
    c != null

  /** `AbstractFileClassLoader#classpathEntries`, called reflectively. */
  private def abstractFileClassLoaderEntries(cl: ClassLoader): Array[String] =
    cl.getClass.getMethod("classpathEntries").nn.invoke(cl).asInstanceOf[Array[String]]

  /** Attempt to recreate a classpath from a classloader.
   *
   *  BEWARE: with exotic enough classloaders, this may not work at all or do
   *  the wrong thing.
   */
  def apply(cl: ClassLoader): String = {
    val classpathBuff = List.newBuilder[String]
    def collectClassLoaderPaths(cl: ClassLoader): Unit = {
      if (cl != null) {
        cl match {
          case cl: URLClassLoader =>
            // This is wrong if we're in a subclass of URLClassLoader
            // that filters loading classes from its parent ¯\_(ツ)_/¯
            collectClassLoaderPaths(cl.getParent)
            // Parent classloaders are searched before their child, so the part of
            // the classpath coming from the child is added at the _end_ of the
            // classpath.
            classpathBuff ++=
              cl.getURLs.iterator.map(url => Paths.get(url.toURI).toAbsolutePath.toString)
          case af: AbstractFileClassLoader =>
            collectClassLoaderPaths(cl.getParent)
            classpathBuff ++= af.classpathEntries
          case _ if isAbstractFileClassLoader(cl) =>
            // HACK: Don't recurse into arbitrary parents: some classloaders hide them on purpose
            // (e.g. sbt's BootFilteredLoader hides the scala-library sbt depends on).
            collectClassLoaderPaths(cl.getParent)
            classpathBuff ++= abstractFileClassLoaderEntries(cl)
          case _ if cl eq ClassLoader.getSystemClassLoader =>
            // HACK: For Java 9+, if the classloader is an AppClassLoader then use the classpath from the system
            // property `java.class.path`.
            classpathBuff += System.getProperty("java.class.path")
          case _ =>
        }
      }
    }
    collectClassLoaderPaths(cl)
    classpathBuff.result().mkString(java.io.File.pathSeparator)
  }
}
