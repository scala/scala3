package dotty.tools.repl

import scala.language.unsafeNulls

import org.junit.Assert.*
import org.junit.Test

class AbstractFileClassLoaderTest:

  import dotty.tools.io.{AbstractFile, VirtualDirectory}
  import scala.collection.mutable.ArrayBuffer
  import scala.io.{Codec, Source}, Codec.UTF8
  import java.io.{BufferedInputStream, Closeable, InputStream}
  import java.net.{URLClassLoader, URL}

  given `we love utf8`: Codec = UTF8

  def closing[T <: Closeable, U](stream: T)(f: T => U): U = try f(stream) finally stream.close()

  extension (f: AbstractFile) def writeContent(s: String): Unit = closing(f.bufferedOutput)(_.write(s.getBytes(UTF8.charSet)))
  def slurp(inputStream: => InputStream)(implicit codec: Codec): String = closing(Source.fromInputStream(inputStream)(codec))(_.mkString)
  def slurp(url: URL)(implicit codec: Codec): String = slurp(url.openStream())

  extension (input: InputStream) def bytes: Array[Byte] =
    val bis = new BufferedInputStream(input)
    val it  = Iterator.continually(bis.read()).takeWhile(_ != -1).map(_.toByte)
    new ArrayBuffer[Byte]().addAll(it).toArray

  // cf ScalaClassLoader#classBytes
  extension (loader: ClassLoader)
    // An InputStream representing the given class name, or null if not found.
    def classAsStream(className: String) = loader.getResourceAsStream {
      if className.endsWith(".class") then className
      else s"${className.replace('.', '/')}.class"  // classNameToPath
    }
    // The actual bytes for a class file, or an empty array if it can't be found.
    def classBytes(className: String): Array[Byte] = classAsStream(className) match
      case null   => Array()
      case stream => stream.bytes

  val NoClassLoader: ClassLoader = null

  // virtual dir "fuzz" and "fuzz/buzz/booz.class"
  def fuzzBuzzBooz: (AbstractFile, AbstractFile) =
    val fuzz = new VirtualDirectory("fuzz", None)
    val buzz = fuzz.subdirectoryNamed("buzz")
    val booz = buzz.fileNamed("booz.class")
    (fuzz, booz)

  @Test def afclGetsParent(): Unit =
    val p = new URLClassLoader(Array.empty[URL])
    val d = new VirtualDirectory("vd", None)
    val x = new AbstractFileClassLoader(d, p)
    assertSame(p, x.getParent)

  @Test def afclGetsResource(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    booz.writeContent("hello, world")
    val sut = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val res = sut.getResource("buzz/booz.class")
    assertNotNull("Find buzz/booz.class", res)
    assertEquals("hello, world", slurp(res))

  @Test def afclGetsResourceFromParent(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz.writeContent("hello, world")
    booz_.writeContent("hello, world_")
    val p = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val sut = new AbstractFileClassLoader(fuzz_, p)
    val res = sut.getResource("buzz/booz.class")
    assertNotNull("Find buzz/booz.class", res)
    assertEquals("hello, world", slurp(res))

  @Test def afclGetsResourceInDefaultPackage(): Unit =
    val fuzz = new VirtualDirectory("fuzz", None)
    val booz = fuzz.fileNamed("booz.class")
    val bass = fuzz.fileNamed("bass")
    booz.writeContent("hello, world")
    bass.writeContent("lo tone")
    val sut = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val res = sut.getResource("booz.class")
    assertNotNull(res)
    assertEquals("hello, world", slurp(res))
    assertEquals("lo tone", slurp(sut.getResource("bass")))

  // scala/bug#8843
  @Test def afclGetsResources(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    booz.writeContent("hello, world")
    val sut = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val e = sut.getResources("buzz/booz.class")
    assertTrue("At least one buzz/booz.class", e.hasMoreElements)
    assertEquals("hello, world", slurp(e.nextElement))
    assertFalse(e.hasMoreElements)

  @Test def afclGetsResourcesFromParent(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz.writeContent("hello, world")
    booz_.writeContent("hello, world_")
    val p = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val x = new AbstractFileClassLoader(fuzz_, p)
    val e = x.getResources("buzz/booz.class")
    assertTrue(e.hasMoreElements)
    assertEquals("hello, world", slurp(e.nextElement))
    assertTrue(e.hasMoreElements)
    assertEquals("hello, world_", slurp(e.nextElement))
    assertFalse(e.hasMoreElements)

  @Test def afclGetsResourceAsStream(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    booz.writeContent("hello, world")
    val x = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val r = x.getResourceAsStream("buzz/booz.class")
    assertNotNull(r)
    assertEquals("hello, world", closing(r)(is => Source.fromInputStream(is).mkString))

  @Test def afclGetsClassBytes(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    booz.writeContent("hello, world")
    val sut = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val b = sut.classBytes("buzz/booz.class")
    assertEquals("hello, world", new String(b, UTF8.charSet))

  @Test def afclGetsClassBytesFromParent(): Unit =
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz.writeContent("hello, world")
    booz_.writeContent("hello, world_")

    val p = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val sut = new AbstractFileClassLoader(fuzz_, p)
    val b = sut.classBytes("buzz/booz.class")
    assertEquals("hello, world", new String(b, UTF8.charSet))
end AbstractFileClassLoaderTest
