package dotty.tools.nio

import org.junit.*
import org.junit.Assert.*
import dotty.tools.io.FileExtension
import dotty.tools.nio.*

import java.nio.charset.StandardCharsets
import scala.io.Codec

class DiskTests:
  private var path: String = ""

  @Before
  def setup(): Unit =
    path = java.nio.file.Files.createTempDirectory("io-tests").toAbsolutePath.toString

  @After
  def teardown(): Unit =
    FileContainer.getOnDisk(path).foreach(_.deleteRecursively())

  private def assertExistsAndDelete(first: String, more: String*): Unit =
    val javaNioPath = java.nio.file.Path.of(first, more*)
    assertTrue(java.nio.file.Files.exists(javaNioPath))
    java.nio.file.Files.delete(javaNioPath)

  private def assertDoesNotExist(first: String, more: String*): Unit =
    assertFalse(java.nio.file.Files.exists(java.nio.file.Path.of(first, more*)))

  private def touch(first: String, more: String*): Unit =
    java.nio.file.Files.createFile(java.nio.file.Path.of(first, more*))

  private def touchDir(first: String, more: String*): Unit =
    java.nio.file.Files.createDirectory(java.nio.file.Path.of(first, more*))

  private def write(contents: Array[Byte], first: String, more: String*): Unit =
    java.nio.file.Files.write(java.nio.file.Path.of(first, more*), contents)

  @Test
  def `FileContainer.getOnDisk finds an existing directory`(): Unit =
    val existing = FileContainer.getOnDisk(path)
    assertTrue(existing.nonEmpty)
    assertEquals(path, existing.get.path)

  @Test
  def `FileContainer.getOnDisk does not find a file`(): Unit =
    touch(path, "f")
    val wrong = FileContainer.getOnDisk(path + FileSystemEntry.separator + "f")
    assertTrue(wrong.isEmpty)

  @Test
  def `FileContainer.getOnDisk does not find a missing directory`(): Unit =
    val missing = FileContainer.getOnDisk(path + "-doesnotexist")
    assertTrue(missing.isEmpty)

  @Test
  def `FileContainer.getOrCreateOnDisk creates a missing directory`(): Unit =
    val newPath = path + "-doesnotexist"
    val missing = FileContainer.getOrCreateOnDisk(newPath)
    assertEquals(missing.path, newPath)
    assertExistsAndDelete(newPath)

  @Test
  def `FileContainer.getOrCreateOnDisk fails on a file`(): Unit =
    touch(path, "f")
    assertThrows(classOf[IllegalArgumentException], () => FileContainer.getOrCreateOnDisk(path + FileSystemEntry.separator + "f"))

  @Test
  def `FileContainer.createTemporaryOnDisk creates a directory`(): Unit =
    val dir = FileContainer.createTemporaryOnDisk("test-name")
    assertTrue(dir.name.contains("test-name"))
    assertExistsAndDelete(dir.path)

  @Test
  def `File.getOnDisk finds an existing file`(): Unit =
    touch(path, "a")
    val existing = File.getOnDisk(path + FileSystemEntry.separator + "a")
    assertTrue(existing.nonEmpty)
    assertEquals(path + FileSystemEntry.separator + "a", existing.get.path)

  @Test
  def `File.getOnDisk does not find a missing file`(): Unit =
    val missing = File.getOnDisk(path + FileSystemEntry.separator + "doesnotexist")
    assertTrue(missing.isEmpty)

  @Test
  def `File.getOnDisk does not find a directory`(): Unit =
    val missing = File.getOnDisk(path)
    assertTrue(missing.isEmpty)

  @Test
  def `File.getOrCreateOnDisk creates a missing file`(): Unit =
    val existing = File.getOrCreateOnDisk(path + FileSystemEntry.separator + "a")
    assertEquals(existing.path, path + FileSystemEntry.separator + "a")
    assertExistsAndDelete(existing.path)

  @Test
  def `File.getOrCreateOnDisk fails on a directory`(): Unit =
    assertThrows(classOf[IllegalArgumentException], () => File.getOrCreateOnDisk(path))

  @Test
  def `DiskDirectory finds direct file entries`(): Unit =
    touch(path, "a")
    touch(path, "b.txt")
    val dir = FileContainer.getOnDisk(path).get

    val entries = dir.entries.toList
    assertEquals(2, entries.length)
    assertTrue(entries.exists(e => e.name == "a" && e.parent == dir))
    assertTrue(entries.exists(e => e.name == "b.txt" && e.parent == dir))

    assertTrue(dir.getFile("a").nonEmpty)
    assertTrue(dir.getContainer("a").isEmpty)
    assertTrue(dir.getFile("b").isEmpty)
    assertTrue(dir.getFile("b", FileExtension.from("txt")).nonEmpty)
    assertTrue(dir.getFile("b", FileExtension.from("other")).isEmpty)

  @Test
  def `DiskDirectory finds directory entries and indirect file entries`(): Unit =
    touchDir(path, "a")
    touch(path, "a", "x.txt")
    val dir = FileContainer.getOnDisk(path).get

    val entries = dir.entries.toList
    assertEquals(1, entries.length)
    assertTrue(entries.exists(_.name == "a"))

    val recursiveEntries = dir.recursiveEntries.toList
    assertEquals(2, recursiveEntries.length)
    assertTrue(recursiveEntries.filter(_.isInstanceOf[FileContainer]).exists(_.name == "a"))
    assertTrue(recursiveEntries.collect {
      case f: File if f.name == "x.txt" && f.nameWithoutExtension == "x" && f.extension == FileExtension.from("txt") => f
    }.nonEmpty)

    assertTrue(dir.getFile("a").isEmpty)
    assertTrue(dir.getContainer("a").nonEmpty)
    assertTrue(dir.getFile("a/x", FileExtension.from("txt"), separator = '/').nonEmpty)
    assertTrue(dir.getFile("a:x.txt", separator = ':').nonEmpty)
    assertTrue(dir.getFile("a:x", separator = ':').isEmpty)

  @Test
  def `DiskDirectory.getOrCreateFile can create a file`(): Unit =
    val container = FileContainer.getOnDisk(path).get
    container.getOrCreateFile("a", FileExtension.from("txt"))
    assertExistsAndDelete(path, "a.txt")

  @Test
  def `DiskDirectory.getOrCreateFile can create a nested file`(): Unit =
    val container = FileContainer.getOnDisk(path).get
    container.getOrCreateFile("a.b.c", FileExtension.from("txt"), separator = '.')
    assertExistsAndDelete(path, "a", "b", "c.txt")
    container.getOrCreateFile("x/y", separator = '/')
    assertExistsAndDelete(path, "x", "y")

  @Test
  def `DiskDirectory.getOrCreateContainer can create a directory`(): Unit =
    val container = FileContainer.getOnDisk(path).get
    container.getOrCreateContainer("a")
    assertExistsAndDelete(path, "a")

  @Test
  def `DiskDirectory.getOrCreateContainer can create a nested directory`(): Unit =
    val container = FileContainer.getOnDisk(path).get
    container.getOrCreateContainer("a.b.c", separator = '.')
    assertExistsAndDelete(path, "a", "b", "c")
    container.getOrCreateContainer("x/y", separator = '/')
    assertExistsAndDelete(path, "x", "y")

  @Test
  def `DiskDirectory can be deleted when empty`(): Unit =
    FileContainer.getOnDisk(path).get.deleteRecursively()
    assertDoesNotExist(path)

  @Test
  def `DiskDirectory can be deleted when nonempty`(): Unit =
    touchDir(path, "a")
    touch(path, "a", "x.txt")
    FileContainer.getOnDisk(path).get.deleteRecursively()
    assertDoesNotExist(path)

  @Test
  def `DiskFile can be deleted`(): Unit =
    touch(path, "a")
    FileContainer.getOnDisk(path).get.getFile("a").get.delete()
    assertDoesNotExist(path, "a")

  @Test
  def `DiskFile's size is accurate`(): Unit =
    touch(path, "a")
    touch(path, "b")
    write(Array(1, 2, 3), path, "b")
    assertEquals(0, FileContainer.getOnDisk(path).get.getFile("a").get.size())
    assertEquals(3, FileContainer.getOnDisk(path).get.getFile("b").get.size())

  @Test
  def `DiskFile's contents are accurate`(): Unit =
    touch(path, "a")
    touch(path, "b")
    write(Array(1, 2, 3), path, "a")
    write(("hello" + System.lineSeparator() + "world").getBytes(StandardCharsets.UTF_8), path, "b")
    assertArrayEquals(Array[Byte](1, 2, 3), FileContainer.getOnDisk(path).get.getFile("a").get.readBytes())
    assertEquals("hello" + System.lineSeparator() + "world", FileContainer.getOnDisk(path).get.getFile("b").get.readText(Codec.UTF8))
    assertEquals(List("hello", "world"), FileContainer.getOnDisk(path).get.getFile("b").get.readLines(Codec.UTF8).toList)

  @Test
  def `DiskFile can be read after being written`(): Unit =
    val container = FileContainer.getOnDisk(path).get
    val a = container.getOrCreateFile("a")
    val b = container.getOrCreateFile("b")
    val c = container.getOrCreateFile("b")

    a.writeBytes(Array(1, 2))
    a.writeBytes(Array(3), append = true)
    assertArrayEquals(Array[Byte](1, 2, 3), a.readBytes())

    b.writeText("hell", Codec.UTF8)
    b.writeLines(List("o", "world"), Codec.UTF8, append = true)
    assertEquals(List("hello", "world"), b.readLines(Codec.UTF8).toList)

    b.copyTo(c)
    assertEquals(List("hello", "world"), b.readLines(Codec.UTF8).toList)

  @Test
  def `DiskFile equality semantics`(): Unit =
    val container = FileContainer.getOnDisk(path).get
    val a1 = container.getOrCreateFile("a", FileExtension.Tasty)
    val a2 = container.getOrCreateFile("a.tasty")
    val b = container.getOrCreateFile("b.txt")
    assertEquals(a1, a2)
    assertEquals(a2, a1)
    assertEquals(a1.hashCode(), a2.hashCode())
    assertNotEquals(b, a1)

  @Test
  def `DiskDirectory equality semantics`(): Unit =
    val container = FileContainer.getOnDisk(path).get
    val a1 = container.getOrCreateContainer("a")
    val a2 = container.getOrCreateContainer("a")
    val b = container.getOrCreateContainer("b")
    assertEquals(a1, a2)
    assertEquals(a2, a1)
    assertEquals(a1.hashCode(), a2.hashCode())
    assertNotEquals(b, a1)
