package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileAlreadyExistsException, FileVisitResult, SimpleFileVisitor, Files as JFiles, Path as JPath}
import scala.jdk.CollectionConverters.*

private object DiskDirectory:
  def get(path: String): Option[FileContainer] =
    val jpath = JPath.of(path).normalize()
    Option.when(JFiles.isDirectory(jpath))(new DiskDirectory(jpath))

  def getOrCreate(path: String): FileContainer =
    val jpath = JPath.of(path).normalize()
    if !JFiles.isDirectory(jpath) then
      try JFiles.createDirectories(jpath)
      catch case e: FileAlreadyExistsException => throw new IllegalArgumentException(s"$path exists but is not a directory")
    new DiskDirectory(jpath)

  def workingDirectory(): FileContainer =
    new DiskDirectory(JPath.of(".").normalize())

  def createTemporary(nameHint: String): FileContainer =
    new DiskDirectory(JFiles.createTempDirectory(nameHint).normalize())

// Invariant: `underlying` is normalized
private final class DiskDirectory(private val underlying: JPath) extends FileContainer:
  override val name: String =
    underlying.getFileName.toString

  override def path: String =
    underlying.toString

  override def parent: FileContainer =
    val underlyingParent = underlying.getParent
    if underlyingParent == null then this
    else new DiskDirectory(underlyingParent)

  override def enclosing: Option[File] =
    None

  override def entries: Iterable[FileSystemEntry] = new Iterable[FileSystemEntry] {
    override def iterator: Iterator[FileSystemEntry] =
      JFiles.list(underlying).map(p => if JFiles.isDirectory(p) then new DiskDirectory(p) else new DiskFile(p)).iterator().asScala
  }

  override def getFile(path: String, extension: FileExtension = FileExtension.Empty, separator: Char = FileSystemEntry.separator): Option[File] =
    val jpath = underlying.resolve(FileContainer.getPath(path, extension, separator)).normalize()
    Option.when(JFiles.isRegularFile(jpath))(new DiskFile(jpath))

  override def getContainer(path: String, separator: Char = FileSystemEntry.separator): Option[FileContainer] =
    val jpath = underlying.resolve(FileContainer.getPath(path, FileExtension.Empty, separator)).normalize()
    Option.when(JFiles.isDirectory(jpath))(new DiskDirectory(jpath))

  override def getOrCreateFile(path: String, extension: FileExtension = FileExtension.Empty, separator: Char = FileSystemEntry.separator): File =
    val jpath = underlying.resolve(FileContainer.getPath(path, extension, separator)).normalize()
    if JFiles.isRegularFile(jpath) then
      new DiskFile(jpath)
    else
      try
        JFiles.createDirectories(jpath.getParent)
        new DiskFile(JFiles.createFile(jpath))
      catch case _: FileAlreadyExistsException => throw new IllegalArgumentException(s"$jpath already exists but is not a file")

  override def getOrCreateContainer(path: String, separator: Char = FileSystemEntry.separator): FileContainer =
    val jpath = underlying.resolve(FileContainer.getPath(path, FileExtension.Empty, separator)).normalize()
    try new DiskDirectory(JFiles.createDirectories(jpath))
    catch case _: FileAlreadyExistsException => throw new IllegalArgumentException(s"$jpath already exists but is not a directory")

  override def deleteRecursively(): Unit =
    try
      JFiles.walkFileTree(underlying, new SimpleFileVisitor[JPath]() {
        override def visitFile(file: JPath, attrs: BasicFileAttributes): FileVisitResult =
          JFiles.delete(file)
          FileVisitResult.CONTINUE
        override def postVisitDirectory(dir: JPath, exc: IOException): FileVisitResult =
          JFiles.delete(dir)
          FileVisitResult.CONTINUE
      })
    catch
      case _: IOException => () // we don't care if something doesn't exist, that's the point

  override def hashCode(): Int =
    underlying.hashCode()

  override def equals(obj: Any): Boolean = obj match
    case otherDiskDirectory: DiskDirectory => underlying.equals(otherDiskDirectory.underlying)
    case _ => false
