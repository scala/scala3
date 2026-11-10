package dotty.tools
package repl

import org.junit.Assert.*
import org.junit.Test

import dotty.tools.nio.{File, FileContainer}
import scala.collection.mutable.ArrayBuffer
import scala.io.{Codec, Source}
import java.io.{BufferedInputStream, BufferedOutputStream, Closeable, InputStream}
import java.net.{URLClassLoader, URL}

import AbstractFileClassLoader.InterruptInstrumentation
import AbstractFileClassLoader.InterruptInstrumentation.*

class InterruptProbe

class AbstractFileClassLoaderTest:

  given `we love utf8`: Codec = Codec.UTF8

  def closing[T <: Closeable, U](stream: T)(f: T => U): U = try f(stream) finally stream.close()

  def slurp(inputStream: => InputStream)(implicit codec: Codec): String = closing(Source.fromInputStream(inputStream)(using codec))(_.mkString)
  def slurp(url: URL)(implicit codec: Codec): String = slurp(url.openStream())

  extension (input: InputStream) def bytes: Array[Byte] =
    val bis = new BufferedInputStream(input)
    val it  = Iterator.continually(bis.read()).takeWhile(_ != -1).map(_.toByte)
    new ArrayBuffer[Byte]().addAll(it).toArray

  // cf ScalaClassLoader#classBytes
  extension (loader: ClassLoader)
    // An InputStream representing the given class name, or null if not found.
    def classAsStream(className: String): InputStream | Null = loader.getResourceAsStream {
      if className.endsWith(".class") then className
      else s"${className.replace('.', '/')}.class"  // classNameToPath
    }
    // The actual bytes for a class file, or an empty array if it can't be found.
    def classBytes(className: String): Array[Byte] = classAsStream(className) match
      case null   => Array()
      case stream => stream.bytes

  // A parent that resolves nothing from the application classpath, so these tests
  // only ever see resources served by the AbstractFileClassLoader under test.
  // `fromURLsParallelCapable` defaults to the platform classloader on Java 9+.
  val noResourcesParent: ClassLoader = ScalaClassLoader.fromURLsParallelCapable(Nil)

  val defaultJavaVersion = ""
  private def newClassLoader(root: FileContainer, parent: ClassLoader) = AbstractFileClassLoader(root, defaultJavaVersion, parent, InterruptInstrumentation.fromString("true"))

  // virtual dir "fuzz" and "fuzz/buzz/booz.class"
  def fuzzBuzzBooz: (FileContainer, File) =
    val fuzz = FileContainer.createInMemory("fuzz")
    val buzz = fuzz.getOrCreateContainer("buzz")
    val booz = buzz.getOrCreateFile("booz.class")
    (fuzz, booz)

  @Test def afclGetsParent(): Unit =
    val p = new URLClassLoader(Array.empty[URL])
    val d = FileContainer.createInMemory("vd")
    val x = newClassLoader(d, p)
    assertSame(p, x.getParent)

  @Test def afclGetsResource(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    booz.writeText("hello, world", Codec.UTF8)
    val sut = newClassLoader(fuzz, noResourcesParent)
    val res = sut.getResource("buzz/booz.class")
    assertNotNull("Find buzz/booz.class", res)
    assertEquals("hello, world", slurp(res))

  @Test def afclGetsResourceFromParent(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz.writeText("hello, world", Codec.UTF8)
    booz_.writeText("hello, world_", Codec.UTF8)
    val p = newClassLoader(fuzz, noResourcesParent)
    val sut = newClassLoader(fuzz_, p)
    val res = sut.getResource("buzz/booz.class")
    assertNotNull("Find buzz/booz.class", res)
    assertEquals("hello, world", slurp(res))

  @Test def afclGetsResourceInDefaultPackage(): Unit =
    val fuzz = FileContainer.createInMemory("fuzz")
    val booz = fuzz.getOrCreateFile("booz.class")
    val bass = fuzz.getOrCreateFile("bass")
    booz.writeText("hello, world", Codec.UTF8)
    bass.writeText("lo tone", Codec.UTF8)
    val sut = newClassLoader(fuzz, noResourcesParent)
    val res = sut.getResource("booz.class")
    assertNotNull(res)
    assertEquals("hello, world", slurp(res))
    assertEquals("lo tone", slurp(sut.getResource("bass")))

  // scala/bug#8843
  @Test def afclGetsResources(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    booz.writeText("hello, world", Codec.UTF8)
    val sut = newClassLoader(fuzz, noResourcesParent)
    val e = sut.getResources("buzz/booz.class")
    assertTrue("At least one buzz/booz.class", e.hasMoreElements)
    assertEquals("hello, world", slurp(e.nextElement))
    assertFalse(e.hasMoreElements)

  @Test def afclGetsResourcesFromParent(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz.writeText("hello, world", Codec.UTF8)
    booz_.writeText("hello, world_", Codec.UTF8)
    val p = newClassLoader(fuzz, noResourcesParent)
    val x = newClassLoader(fuzz_, p)
    val e = x.getResources("buzz/booz.class")
    assertTrue(e.hasMoreElements)
    assertEquals("hello, world", slurp(e.nextElement))
    assertTrue(e.hasMoreElements)
    assertEquals("hello, world_", slurp(e.nextElement))
    assertFalse(e.hasMoreElements)

  @Test def afclGetsResourceAsStream(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    booz.writeText("hello, world", Codec.UTF8)
    val x = newClassLoader(fuzz, noResourcesParent)
    val r = x.getResourceAsStream("buzz/booz.class")
    assertNotNull(r)
    assertEquals("hello, world", closing(r.nn)(is => Source.fromInputStream(is).mkString))

  @Test def afclGetsClassBytes(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    booz.writeText("hello, world", Codec.UTF8)
    val sut = newClassLoader(fuzz, noResourcesParent)
    val b = sut.classBytes("buzz/booz.class")
    assertEquals("hello, world", new String(b, Codec.UTF8.charSet))

  @Test def afclGetsClassBytesFromParent(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz.writeText("hello, world", Codec.UTF8)
    booz_.writeText("hello, world_", Codec.UTF8)
    val p = newClassLoader(fuzz, noResourcesParent)
    val sut = newClassLoader(fuzz_, p)
    val b = sut.classBytes("buzz/booz.class")
    assertEquals("hello, world", new String(b, Codec.UTF8.charSet))

  def probeIsInterruptible(
      mode: InterruptInstrumentation,
      parent: ClassLoader = classOf[InterruptProbe].getClassLoader
  ): Boolean =
    val name = classOf[InterruptProbe].getName
    val root = FileContainer.createInMemory("replout")
    val dir = root.getOrCreateContainer(name, separator = '.')
    closing(dir.getOrCreateFile("InterruptProbe.class").output())(_.write(classOf[InterruptProbe].getClassLoader.classBytes(name)))
    val loader = new AbstractFileClassLoader(root, defaultJavaVersion, parent, mode)
    ReplBytecodeInstrumentation.setStopFlag(loader, true)
    try
      loader.loadClass(name).getDeclaredConstructor().newInstance()
      false
    catch case e: java.lang.reflect.InvocationTargetException => e.getCause.isInstanceOf[ThreadDeath]
    finally ReplBytecodeInstrumentation.setStopFlag(loader, false)

  @Test def replClassesAreNotInstrumentedWhenDisabled(): Unit =
    assertFalse(probeIsInterruptible(Disabled))

  @Test def replClassesAreInstrumentedWhenEnabled(): Unit =
    assertTrue(probeIsInterruptible(Enabled))

  @Test def replClassesAreInstrumentedWhenLocal(): Unit =
    assertTrue(probeIsInterruptible(Local))

  @Test def localModeWorksWhenParentCannotSeeStopRepl(): Unit =
    assertTrue(probeIsInterruptible(Local, ClassLoader.getPlatformClassLoader))

end AbstractFileClassLoaderTest
