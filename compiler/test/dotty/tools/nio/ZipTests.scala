package dotty.tools.nio

import org.junit.*
import org.junit.Assert.*
import dotty.tools.io.FileExtension
import dotty.tools.nio.*

import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.io.Codec
import scala.jdk.CollectionConverters.*

class ZipTests:
  protected val COMPRESSION = 1
  protected var path: String = ""

  protected def extension: String = ".zip"

  @Before
  def setup(): Unit =
    path = java.nio.file.Files.createTempFile("archive-tests", extension).toAbsolutePath.toString
    teardown() // we just want the path we don't want it to exist yet

  @After
  def teardown(): Unit =
    java.nio.file.Files.deleteIfExists(java.nio.file.Path.of(path))

  private def listFiles(): Map[String, String] =
    assertTrue(java.nio.file.Files.exists(java.nio.file.Path.of(path)))
    try
      val archive = new java.util.zip.ZipFile(path)
      try
        archive.entries().asScala
          .filterNot(_.isDirectory)
          .map(e => e.getName -> {
            val is = archive.getInputStream(e)
            try new String(is.readAllBytes(), StandardCharsets.UTF_8)
            finally is.close()
          })
          .toMap
      finally
        archive.close()
    catch
      case e if e.getMessage.contains("zip file is empty") => Map.empty

  protected def createArchive(entries: Map[String, String], compressionLevel: Int = COMPRESSION): Unit =
    val os = new java.util.zip.ZipOutputStream(new java.io.FileOutputStream(path))
    os.setLevel(compressionLevel)
    if compressionLevel == 0 then os.setMethod(java.util.zip.ZipOutputStream.STORED)
    val dirs = mutable.HashSet.empty[String]
    def addEntry(name: String, contents: String): Unit =
      var idx = name.indexOf('/')
      while idx != -1 do
        val p = name.substring(0, idx)
        if dirs.add(p) then
          val dirEntry = new java.util.zip.ZipEntry(p + "/")
          if compressionLevel == 0 then
            dirEntry.setSize(0)
            dirEntry.setCompressedSize(0)
            dirEntry.setCrc(0)
          os.putNextEntry(dirEntry)
          os.closeEntry()
        idx = name.indexOf('/', idx + 1)
      val entry = new java.util.zip.ZipEntry(name)
      val entryContents = contents.getBytes(StandardCharsets.UTF_8)
      if compressionLevel == 0 then
        entry.setSize(entryContents.length)
        val crc = new java.util.zip.CRC32()
        crc.update(entryContents)
        entry.setCrc(crc.getValue)
      os.putNextEntry(entry)
      os.write(entryContents)
      os.flush()
      os.closeEntry()
    try
      for (name, contents) <- entries do addEntry(name, contents)
    finally
      os.close()

  protected def open(compressionLevel: Int = COMPRESSION): FileContainer =
    val underlying = File.getOrCreateOnDisk(path)
    FileContainer.getFromFile(underlying, jarVersion = "", compressionLevel = compressionLevel).get

  protected def manifestName(): Set[String] =
    Set.empty

  protected def assertManifestContents(map: Map[String, String]): Unit =
    ()

  @Test
  def `Creating an empty archive`(): Unit =
    val archive = open()
    archive.close()
    assertTrue(listFiles().isEmpty)

  @Test
  def `Creating a non-empty archive`(): Unit =
    val archive = open()
    archive.getOrCreateFile("X", FileExtension.Class).writeText("Hello, World!", Codec.UTF8)
    val sub = archive.getOrCreateContainer("D").getOrCreateFile("Y", FileExtension.Tasty)
    sub.writeText("I'm", Codec.UTF8)
    sub.writeText(" tasty!", Codec.UTF8, append = true)
    archive.close()
    val onDisk = listFiles()
    assertEquals(manifestName() ++ Set("X.class", "D/Y.tasty"), onDisk.keys)
    assertManifestContents(onDisk)
    assertEquals("Hello, World!", onDisk("X.class"))
    assertEquals("I'm tasty!", onDisk("D/Y.tasty"))

  @Test
  def `Adding files to an existing archive`(): Unit =
    createArchive(Map("A.class" -> "Hello"))
    val archive = open()
    archive.getOrCreateContainer("X").getOrCreateFile("B.class").writeText("World!", Codec.UTF8)
    archive.close()
    val onDisk = listFiles()
    assertEquals(manifestName() ++ Set("A.class", "X/B.class"), onDisk.keys)
    assertManifestContents(onDisk)
    assertEquals("Hello", onDisk("A.class"))
    assertEquals("World!", onDisk("X/B.class"))

  @Test
  def `Modifying files in an existing archive`(): Unit =
    createArchive(Map("A.class" -> "Hello", "X/B.tasty" -> "World!"))
    val archive = open()
    archive.getFile("A.class").get.writeText(" World!", Codec.UTF8, append = true)
    archive.getContainer("X").get.getFile("B", FileExtension.Tasty).get.writeText("R", Codec.UTF8)
    archive.close()
    val onDisk = listFiles()
    assertEquals(manifestName() ++ Set("A.class", "X/B.tasty"), onDisk.keys)
    assertManifestContents(onDisk)
    assertEquals("Hello World!", onDisk("A.class"))
    assertEquals("R", onDisk("X/B.tasty"))

  @Test
  def `Deleting files from an existing archive`(): Unit =
    createArchive(Map("A.class" -> "Hello", "B.class" -> "World!", "X/C.tasty" -> "nested"))
    val archive = open()
    archive.getFile("A", FileExtension.Class).get.delete()
    archive.getContainer("X").get.deleteRecursively()
    archive.close()
    val onDisk = listFiles()
    assertEquals(manifestName() ++ Set("B.class"), onDisk.keys)
    assertEquals("World!", onDisk("B.class"))

  @Test
  def `Reading from an existing archive`(): Unit =
    createArchive(Map("A.class" -> "Hello", "X/B.tasty" -> "World!", "X/Y/C.txt" -> "Text"))
    val archive = open()
    val a = archive.getFile("A.class")
    assertTrue(a.nonEmpty)
    assertEquals(archive, a.get.parent)
    assertEquals("Hello", a.get.readText(Codec.UTF8))
    val x = archive.getContainer("X")
    assertTrue(x.nonEmpty)
    assertEquals(archive, x.get.parent)
    val b = x.get.getFile("B", FileExtension.Tasty)
    assertTrue(b.nonEmpty)
    assertEquals(x.get, b.get.parent)
    assertEquals("World!", b.get.readText(Codec.UTF8))
    assertTrue(archive.getFile("X/B.tasty", separator = '/').nonEmpty)
    val c = archive.getFile("X.Y.C", FileExtension.from("txt"), separator = '.')
    assertTrue(c.nonEmpty)
    val y = archive.getContainer("X/Y", separator = '/')
    assertTrue(y.nonEmpty)
    assertEquals(List(b.get, y.get), x.get.entries.toList)
    assertEquals(List(b.get, y.get, c.get), x.get.recursiveEntries.toList)
    archive.close()

  @Test
  def `The path of an entry is compatible with what other tools expect`(): Unit =
    createArchive(Map("X/A.class" -> "Hello"))
    val archive = open()
    assertEquals(path + "!X/A.class", archive.getFile("X.A", FileExtension.Class, separator = '.').get.path)

  @Test
  def `Read and write without compression`(): Unit =
    createArchive(Map("A.class" -> "Hello", "X/B.tasty" -> "World!"), compressionLevel = 0)
    val archive = open(0)
    assertEquals("Hello", archive.getFile("A.class").get.readText(Codec.UTF8))
    archive.getContainer("X").get.getFile("B", FileExtension.Tasty).get.writeText("R", Codec.UTF8)
    archive.close()
    val onDisk = listFiles()
    assertEquals("R", onDisk("X/B.tasty"))