package scala.tools.repl

import java.net.{URL, URLClassLoader}
import java.io.InputStream

/**
 * A classloader that remaps shaded classes back to their original package names.
 *
 * This classloader intercepts class loading requests and remaps them from
 * dotty.tools.repl.shaded.* back to their original package names, allowing the
 * shaded classes to be loaded as if they were in their original packages.
 *
 * The scala.* packages are not shaded, so they pass through normally.
 */
class UnshadingClassLoader(parent: ClassLoader) extends ClassLoader(parent) {

  private val SHADED_PREFIX = "dotty.tools.repl.shaded."

  override def loadClass(name: String, resolve: Boolean): Class[?] = {
    val loaded = findLoadedClass(name)
    if (loaded != null) return loaded

    val shadedPath = (SHADED_PREFIX + name).replace('.', '/') + ".class"
    val is0 = try {
      Option(super.getResourceAsStream(shadedPath))
    }catch{
      case _: Exception => None
    }

    is0 match{
      case Some(is) =>
        try {
          val bytes = is.readAllBytes()
          val clazz = defineClass(name, bytes, 0, bytes.length)
          if (resolve) resolveClass(clazz)
          return clazz
        } finally is.close()
      case None => super.loadClass(name, resolve)
    }
  }

  override def getResourceAsStream(name: String): InputStream | Null = {
    val shadedPath = SHADED_PREFIX.replace('.', '/') + name
    val shadedStream = super.getResourceAsStream(shadedPath)
    if (shadedStream != null) return shadedStream
    else super.getResourceAsStream(name)
  }
}

/**
 * Main entry point for the embedded shaded REPL.
 *
 * This creates an isolated classloader that loads the shaded REPL classes
 * as if they were unshaded, instantiates a ReplDriver, and runs it.
 */
object EmbeddedReplMain {
  def main(args: Array[String]): Unit = {
    val argsWithClasspath =
      if (args.exists(arg => arg == "-classpath" || arg == "-cp")) args
      else Array("-classpath", System.getProperty("java.class.path")) ++ args

    val unshadingClassLoader = new UnshadingClassLoader(getClass.getClassLoader)

    val replDriverClass = unshadingClassLoader.loadClass("dotty.tools.repl.ReplDriver")

    val someCls = unshadingClassLoader.loadClass("scala.Some")
    // Create the ReplDriver instance with classpath argument
    val replDriver = replDriverClass.getConstructors().head.newInstance(
      argsWithClasspath, // settings: Array[String] (now includes -classpath)
      System.out, // out: PrintStream
      someCls.getConstructors().head.newInstance(getClass.getClassLoader),
      "" // extraPredef: String
    )

    replDriverClass.getMethod("tryRunning").invoke(replDriver)

  }
}
