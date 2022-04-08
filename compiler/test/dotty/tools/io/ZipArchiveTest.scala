package dotty.tools.io

import scala.language.unsafeNulls

import java.io.IOException
import java.net.{URI, URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{Attributes, Manifest, JarEntry, JarOutputStream}
import java.lang.invoke.{MethodHandles, MethodType}

import org.junit.Assert._
import org.junit.Test

import scala.util.chaining._
import scala.util.Using

class ZipArchiveTest {

  @Test
  def corruptZip(): Unit = {
    val f = Files.createTempFile("test", ".jar")
    val fza = new FileZipArchive(f, release = None)
    try {
      fza.iterator
      assert(false)
    }
    catch {
      case ex: IOException =>
    }
    finally {
      Files.delete(f)
    }
  }

  @Test
  def missingFile(): Unit = {
    val f = Paths.get("xxx.does.not.exist")
    val fza = new FileZipArchive(f, release = None)
    try {
      fza.iterator
      assert(false)
    }
    catch {
      case ex: IOException =>
    }
  }

  private val bootClassLoader: ClassLoader = {
    if (!util.Properties.isJavaAtLeast("9")) null
    else {
      try {
        MethodHandles
          .lookup()
          .findStatic(
            classOf[ClassLoader],
            "getPlatformClassLoader",
            MethodType.methodType(classOf[ClassLoader])
          )
          .invoke()
          .asInstanceOf[ClassLoader]
      } catch {
        case _: Throwable =>
          null
      }
    }
  }

  private def classLoader(location: URI): ClassLoader =
    new URLClassLoader(Array(location.toURL), bootClassLoader)

  private def manifestAt(location: URI): URL = classLoader(location).getResource("META-INF/MANIFEST.MF")


  // ZipArchive.fromManifestURL(URL)
  @Test def `manifest resources just works`(): Unit = {
    val jar = createTestJar()
    val archive = new ManifestResources(manifestAt(jar.toUri))
    try {
      val it = archive.iterator
      assertTrue(it.hasNext)
      val f = it.next()
      assertFalse(it.hasNext)
      assertEquals("foo.class", f.name)
    }
    finally {
      archive.close()
      // The following results in IOException on Windows (file in use by another process).
      // As jar created with Files.createTempFile, it will be deleted automatically.
      try Files.delete(jar) catch case _: IOException => ()
    }
  }

  private def createTestJar(): Path = Files.createTempFile("junit", ".jar").tap { f =>
    val man = new Manifest()
    man.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0")
    man.getEntries().put("foo.class", new Attributes(0))
    Using.resource(new JarOutputStream(Files.newOutputStream(f), man)) { jout =>
      jout.putNextEntry(new JarEntry("foo.class"))
      val bytes = "hello, world".getBytes
      jout.write(bytes, 0, bytes.length)
      ()
    }
  }

  private def createTestZip(): Path = Files.createTempFile("junit", ".zip").tap { f =>
    import java.util.zip._
    Using.resource(new ZipOutputStream(Files.newOutputStream(f))) { zout =>
      zout.setLevel(Deflater.NO_COMPRESSION)
      zout.setMethod(ZipOutputStream.STORED)
      val entry = new ZipEntry("foo.class")
      val bytes = "hello, world".getBytes
      entry.setSize(bytes.length)
      entry.setCompressedSize(bytes.length)
      entry.setCrc(new CRC32().tap(_.update(bytes, 0, bytes.length)).getValue)
      zout.putNextEntry(entry)
      zout.write(bytes, 0, bytes.length)
      zout.closeEntry()
      ()
    }
  }
  /* zipfs doesn't write size field in file header as required by URLZipArchive
  private def createTestZip2(): Path = {
    import java.nio.file.FileSystems
    import java.net.URI
    import scala.util.chaining._
    import scala.jdk.CollectionConverters._
    val f = Files.createTempFile("junit", ".zip")
    Files.delete(f)
    val uri = URI.create(s"jar:${f.toUri}")
    val env = Map("create" -> "true").asJava
    Using.resource(FileSystems.newFileSystem(uri, env)) { fs =>
      val path = fs.getPath("foo.class")
      val bytes = "hello, world".getBytes
      Files.write(path, bytes)
    }
    f.tap(println(_))
  }
   */
}
