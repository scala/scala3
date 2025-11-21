package dotty.embedded

import java.net.{URL, URLClassLoader}
import java.io.InputStream

/**
 * A classloader that remaps shaded classes back to their original package names.
 */
class UnshadingClassLoader(parent: ClassLoader) extends ClassLoader(parent) {

  // dotty.isolated classes are loaded only within the REPL impl classloader.
  // They exist in the enclosing classpath relocated within the dotty.isolated
  // package, but are relocated to their proper package when the REPL impl
  // classloader loads them
  private val ISOLATED_PREFIX = "dotty.isolated."

  override def loadClass(name: String, resolve: Boolean): Class[?] = {
    val loaded = findLoadedClass(name)
    if (loaded != null) return loaded

    // dotty.shaded classes are loaded separately between the REPL line classloader
    // and the REPL impl classloader, but at the same path because the REPL line
    // classloader doesn't tolerate relocating classfiles
    val shadedPath = (if (name.startsWith("dotty.shaded.")) name else ISOLATED_PREFIX + name)
      .replace('.', '/') + ".class"

    val is0 = scala.util.Try(Option(super.getResourceAsStream(shadedPath))).toOption.flatten

    is0 match{
      case Some(is) =>
        try {
          val bytes = is.readAllBytes()
          val clazz = defineClass(name, bytes, 0, bytes.length)
          if (resolve) resolveClass(clazz)
          clazz
        } finally is.close()
      case None =>
        // These classes are loaded shared between all classloaders, because
        // they misbehave if loaded multiple times in separate classloaders
        if (name.startsWith("java.") || name.startsWith("org.jline.")) parent.loadClass(name)
        // Other classes loaded by the `UnshadingClassLoader` *must* be found in the
        // `dotty.isolated` package. If they're not there, throw an error rather than
        // trying to look for them at their normal package path, to ensure we're not
        // accidentally pulling stuff in from the enclosing classloader
        else throw new ClassNotFoundException(name)
    }
  }

  override def getResourceAsStream(name: String): InputStream | Null = {
    super.getResourceAsStream(ISOLATED_PREFIX.replace('.', '/') + name)
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
    val pprintImport = replDriverClass.getMethod("pprintImport").invoke(null)

    val replDriver = replDriverClass.getConstructors().head.newInstance(
      /*settings*/ argsWithClasspath,
      /*out*/ System.out,
      /*classLoader*/ someCls.getConstructors().head.newInstance(getClass.getClassLoader),
      /*extraPredef*/ pprintImport
    )

    replDriverClass.getMethod("tryRunning").invoke(replDriver)
  }
}
