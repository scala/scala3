package dotty.tools.nio

import org.junit.*
import org.junit.Assert.*
import dotty.tools.io.FileExtension
import dotty.tools.nio.*

import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.io.Codec

class JarTests extends ZipTests:
  private val VERSION = "17"

  override protected def extension: String = ".jar"

  override protected def createArchive(entries: Map[String, String], compressionLevel: Int = COMPRESSION): Unit =
    createJar(entries, compressionLevel)

  override protected def open(compressionLevel: Int = COMPRESSION): FileContainer =
    val underlying = File.getOrCreateOnDisk(path)
    FileContainer.getFromFile(underlying, jarVersion = VERSION, compressionLevel = compressionLevel).get

  override protected def manifestName(): Set[String] =
    Set("META-INF/MANIFEST.MF")

  override protected def assertManifestContents(map: Map[String, String]): Unit =
    assertEquals("Manifest-Version: 1.0\r\n\r\n", map("META-INF/MANIFEST.MF"))

  private def createJar(entries: Map[String, String], compressionLevel: Int = COMPRESSION, multiRelease: Boolean = false): Unit =
    val manifest = new java.util.jar.Manifest()
    manifest.getMainAttributes.put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0")
    if multiRelease then manifest.getMainAttributes.put(java.util.jar.Attributes.Name.MULTI_RELEASE, "true")
    val jos = new java.util.jar.JarOutputStream(new java.io.FileOutputStream(path), manifest)
    jos.setLevel(compressionLevel)
    if compressionLevel == 0 then jos.setMethod(java.util.zip.ZipOutputStream.STORED)
    val dirs = mutable.HashSet.empty[String]
    def addEntry(name: String, contents: String): Unit =
      var idx = name.indexOf('/')
      while idx != -1 do
        val p = name.substring(0, idx)
        if dirs.add(p) then
          val dirEntry = new java.util.jar.JarEntry(p + "/")
          if compressionLevel == 0 then
            dirEntry.setSize(0)
            dirEntry.setCompressedSize(0)
            dirEntry.setCrc(0)
          jos.putNextEntry(dirEntry)
          jos.closeEntry()
        idx = name.indexOf('/', idx + 1)
      val entry = new java.util.jar.JarEntry(name)
      val entryContents = contents.getBytes(StandardCharsets.UTF_8)
      if compressionLevel == 0 then
        entry.setSize(entryContents.length)
        val crc = new java.util.zip.CRC32()
        crc.update(entryContents)
        entry.setCrc(crc.getValue)
      jos.putNextEntry(entry)
      jos.write(entryContents)
      jos.flush()
      jos.closeEntry()
    try
      for (name, contents) <- entries do addEntry(name, contents)
    finally
      jos.close()

  @Test
  def `For multi-release JARs, the right file releases are selected`(): Unit =
    createJar(Map(
      "A.class" -> "too early", "META-INF/versions/9/A.class" -> "ok", "META-INF/versions/9999/A.class" -> "too late",
      "B.class" -> "ok", "META-INF/versions/9999/B.class" -> "too late",
      "X/C.class" -> "too early", "META-INF/versions/9/X/C.class" -> "ok",
      "META-INF/versions/9/D.class" -> "ok",
    ), multiRelease = true)
    val underlying = File.getOrCreateOnDisk(path)
    val jar = FileContainer.getFromFile(underlying, VERSION, COMPRESSION).get
    assertEquals("ok", jar.getFile("A.class").get.readText(Codec.UTF8))
    assertEquals("ok", jar.getFile("B", FileExtension.Class).get.readText(Codec.UTF8))
    assertEquals("ok", jar.getFile("X/C.class", separator = '/').get.readText(Codec.UTF8))
    assertEquals("ok", jar.getFile("D.class").get.readText(Codec.UTF8))
    jar.close()
