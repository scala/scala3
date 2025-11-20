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

  private val SHADED_PACKAGES = Seq("dotty.", "org.", "com.", "io.", "coursier.", "coursierapi.", "dependency.", "pprint.", "fansi.", "sourcecode.", "xsbti.")

  private val UNSHADED_PACKAGES = Seq("scala.", "scala.tools.repl.", "org.jline.")

  /** Check if a class/resource name should be loaded from the shaded location */
  private def shouldUnshade(name: String): Boolean = {
    SHADED_PACKAGES.exists(pkg => name.startsWith(pkg)) &&
    !name.startsWith(SHADED_PREFIX) &&
    !UNSHADED_PACKAGES.exists(pkg => name.startsWith(pkg)) ||
    name.startsWith("scala.tools.asm")
  }

  override def loadClass(name: String, resolve: Boolean): Class[?] = {
    if (shouldUnshade(name)) {
      val loaded = findLoadedClass(name)
      if (loaded != null) return loaded

      try {
        val shadedPath = (SHADED_PREFIX + name).replace('.', '/') + ".class"
        val is = getParent.getResourceAsStream(shadedPath)

        if (is != null) {
          try {
            val bytes = is.readAllBytes()
            val clazz = defineClass(name, bytes, 0, bytes.length)
            if (resolve) resolveClass(clazz)
            return clazz
          } finally is.close()
        }
      } catch {
        case _: Exception => // Fall through to parent
      }
    }

    super.loadClass(name, resolve)
  }

  override def getResourceAsStream(name: String): InputStream | Null = {
    val nameAsDots = name.replace('/', '.')

    if (shouldUnshade(nameAsDots)) {
      val shadedPath = SHADED_PREFIX.replace('.', '/') + name
      val shadedStream = super.getResourceAsStream(shadedPath)
      if (shadedStream != null) return shadedStream
    }

    super.getResourceAsStream(name)
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
    val constructor = replDriverClass.getConstructors().head

    // Create the ReplDriver instance with classpath argument
    val replDriver = constructor.newInstance(
      argsWithClasspath, // settings: Array[String] (now includes -classpath)
      System.out, // out: PrintStream
      Option(getClass.getClassLoader), // classLoader: Option[ClassLoader]
      "" // extraPredef: String
    )

    replDriverClass.getMethod("tryRunning").invoke(replDriver)

  }
}
