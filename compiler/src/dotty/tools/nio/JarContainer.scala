package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.{InputStream, OutputStream}
import java.util.jar.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.IteratorHasAsScala

object JarContainer:
  def open(file: File, version: String): FileContainer = file match
    case disk: DiskFile => new JarContainer(disk, version)
    case _ => throw new UnsupportedOperationException("Loading JARs from outside the disk is not supported yet.")

private final class JarContainer private(val underlying: File, version: String) extends FileContainer:
  lazy val jarFile = new JarFile(
    new java.io.File(underlying.path),
    true, // default
    java.util.zip.ZipFile.OPEN_READ, // default
    if version == "" then Runtime.version() else Runtime.Version.parse(version)
  )
  // None = delete
  val modifications: mutable.Map[String, Option[JarEntry]] = mutable.Map.empty

  override def name: String =
    underlying.name

  override def path: String =
    underlying.path

  override def parent: FileContainer =
    underlying.parent

  override def enclosing: Option[File] =
    None

  override def entries: Iterable[FileSystemEntry] = new Iterable[FileSystemEntry] {
    def iterator: Iterator[FileSystemEntry] =
      jarFile.versionedStream().iterator().asScala.map(e =>
        if e.isDirectory then new JarEntryContainer(JarContainer.this, e) else new JarEntryFile(JarContainer.this, e)
      )
  }

  override def deleteRecursively(): Unit =
    jarFile.close()
    underlying.delete()

  override def close(): Boolean = {
    if modifications.nonEmpty then
      ??? // TODO add entries and write
    jarFile.close()
    true
  }

  // TODO share logic between this + sub-container
  protected override def getFile(name: String, extension: FileExtension): Option[File] =
    ???

  protected override def getContainer(name: String): Option[FileContainer] =
    ???

  protected override def createFile(name: String, extension: FileExtension): File =
    ???

  protected override def createContainer(name: String): FileContainer =
    ???

private trait JarEntryFileSystemEntry(container: JarContainer, entry: JarEntry) extends FileSystemEntry:
  override def name: String =
    val idx = path.lastIndexOf('/')
    if idx == -1 then path else path.substring(idx + 1)

  override def path: String =
    entry.getName // is actually the path!

  override def enclosing: Option[File] =
    Some(container.underlying)


private final class JarEntryContainer(container: JarContainer, entry: JarEntry) extends FileContainer, JarEntryFileSystemEntry(container, entry):
  override def parent: FileContainer =
    ???

  override def entries: Iterable[FileSystemEntry] =
    ???

  override def deleteRecursively(): Unit =
    ???

  protected override def getFile(name: String, extension: FileExtension): Option[File] =
    ???

  protected override def getContainer(name: String): Option[FileContainer] =
    ???

  protected override def createFile(name: String, extension: FileExtension): File =
    ???

  protected override def createContainer(name: String): FileContainer =
    ???

private final class JarEntryFile(container: JarContainer, entry: JarEntry) extends File, JarEntryFileSystemEntry(container, entry):
  override def parent: FileContainer =
    ???

  override def lastModified(): Long =
    entry.getLastModifiedTime.toMillis

  override def size(): Long =
    entry.getSize

  override def input(): InputStream =
    container.jarFile.getInputStream(entry)

  override def output(append: Boolean = false): OutputStream =
    ???

  override def delete(): Unit =
    container.modifications(path) = None