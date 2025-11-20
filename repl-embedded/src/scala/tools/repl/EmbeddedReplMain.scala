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

  // Packages that were shaded
  private val SHADED_PACKAGES = Seq("dotty.", "org.", "com.", "io.", "coursier.", "coursierapi.", "dependency.", "pprint.", "fansi.", "sourcecode.", "xsbti.")

  override def loadClass(name: String, resolve: Boolean): Class[?] = {
    // Check if this is a class from a package we shaded (and not already in the shaded package)
    val shouldUnshade = SHADED_PACKAGES.exists(pkg => name.startsWith(pkg)) &&
                        !name.startsWith(SHADED_PREFIX)

    if (shouldUnshade) {
      // Try to find the shaded version
      val shadedName = SHADED_PREFIX + name

      // First check if we've already loaded this class
      val loaded = findLoadedClass(name)
      if (loaded != null) return loaded

      try {
        // Load the shaded class bytes from parent
        val resourceName = shadedName.replace('.', '/') + ".class"
        val is = getParent.getResourceAsStream(resourceName)

        if (is != null) {
          try {
            // Read the class bytes
            val bytes = readAllBytes(is)

            // Define the class with the unshaded name
            val clazz = defineClass(name, bytes, 0, bytes.length)
            if (resolve) resolveClass(clazz)
            return clazz
          } finally {
            is.close()
          }
        }
      } catch {
        case _: Exception => // Fall through to parent
      }
    }

    // For everything else (scala.* and already shaded classes), delegate to parent
    super.loadClass(name, resolve)
  }

  private def readAllBytes(is: InputStream): Array[Byte] = {
    val buffer = new Array[Byte](8192)
    var bytesRead = 0
    val baos = new java.io.ByteArrayOutputStream()

    while ({bytesRead = is.read(buffer); bytesRead != -1}) {
      baos.write(buffer, 0, bytesRead)
    }

    baos.toByteArray()
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
    // Get the location of the current jar to use as classpath
    val codeSource = getClass.getProtectionDomain.getCodeSource
    val jarPath = if (codeSource != null) {
      val location = codeSource.getLocation
      if (location.getProtocol == "file") {
        new java.io.File(location.toURI).getAbsolutePath
      } else {
        location.toString
      }
    } else {
      // Fallback: try to extract from classpath
      System.getProperty("java.class.path")
    }

    // Add -classpath argument pointing to the shaded jar itself
    // This allows the ReplDriver's compiler to find scala.* classes
    val argsWithClasspath = if (args.exists(arg => arg == "-classpath" || arg == "-cp")) {
      args // Already has classpath
    } else {
      Array("-classpath", jarPath) ++ args
    }

    // Create the unshading classloader with the current classloader as parent
    // This ensures it has access to all dependencies in the shaded jar
    val unshadingClassLoader = new UnshadingClassLoader(
      getClass.getClassLoader // Use current classloader to access all dependencies
    )

    // Load the ReplDriver class through the unshading classloader
    val replDriverClass = unshadingClassLoader.loadClass("dotty.tools.repl.ReplDriver")

    // Get the constructor: ReplDriver(Array[String], PrintStream, Option[ClassLoader], String)
    val constructor = replDriverClass.getConstructors()(0)

    // Create an Option[ClassLoader] containing the external classloader
    val scalaOptionClass = unshadingClassLoader.loadClass("scala.Option")
    val scalaOptionModule = unshadingClassLoader.loadClass("scala.Option$")
    val someMethod = scalaOptionModule.getField("MODULE$").get(null)
      .asInstanceOf[Object].getClass.getMethod("apply", classOf[Object])
    val classLoaderOption = someMethod.invoke(
      scalaOptionModule.getField("MODULE$").get(null),
      getClass.getClassLoader // Pass the external classloader
    )

    // Create the ReplDriver instance with classpath argument
    val replDriver = constructor.newInstance(
      argsWithClasspath,        // settings: Array[String] (now includes -classpath)
      System.out,               // out: PrintStream
      classLoaderOption,        // classLoader: Option[ClassLoader]
      ""                        // extraPredef: String
    )

    // Call tryRunning on the ReplDriver
    val tryRunningMethod = replDriverClass.getMethod("tryRunning")
    tryRunningMethod.invoke(replDriver)
  }
}
