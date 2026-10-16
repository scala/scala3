package dotty.tools.dotc.util

import java.net.URLClassLoader
import java.nio.file.Paths

import dotty.tools.io
import org.junit.Test
import org.junit.Assert.*

class ClasspathFromClassloaderTest:
  @Test def virtualOutputPreservesParentClasspath(): Unit =
    val dependency = Paths.get("parent-dependency.jar").toAbsolutePath
    // Classpath reconstruction needs only the URL, not an actual archive.
    val parent = new URLClassLoader(Array(dependency.toUri.toURL), null)
    try
      val child = new io.AbstractFileClassLoader(io.virtualDirectory("output"), parent)
      assertEquals(dependency.toString, ClasspathFromClassloader(parent))
      assertEquals(dependency.toString, ClasspathFromClassloader(child))
    finally parent.close()
