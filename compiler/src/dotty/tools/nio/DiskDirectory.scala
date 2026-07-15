package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, SimpleFileVisitor, Files as JFiles, Path as JPath}
import scala.jdk.CollectionConverters.*

private[nio] object DiskDirectory:
  def get(path: String): Option[FileContainer] =
    val jpath = JPath.of(path).normalize()
    Option.when(JFiles.isDirectory(jpath))(new DiskDirectory(jpath))

  def getOrCreate(path: String): FileContainer =
    val jpath = JPath.of(path).normalize()
    if !JFiles.isDirectory(jpath) then
      JFiles.createDirectories(jpath)
    new DiskDirectory(jpath)

  def workingDirectory(): FileContainer =
    new DiskDirectory(JPath.of(".").normalize())

  def createTemporary(nameHint: String): FileContainer =
    new DiskDirectory(JFiles.createTempDirectory(nameHint))

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

  /** Gets the file in this container with the given name and extension if it exists. */
  protected override def getFile(name: String, extension: FileExtension): Option[File] =
    val res = underlying.resolve(name + extension.withDot)
    Option.when(JFiles.isRegularFile(res))(new DiskFile(res))

  /** Gets the container in this container with the given name if it exists. */
  protected override def getContainer(name: String): Option[FileContainer] =
    val res = underlying.resolve(name).normalize()
    Option.when(JFiles.isDirectory(res))(new DiskDirectory(res))

  /** Creates a file in this container with the given name and extension. */
  protected override def createFile(name: String, extension: FileExtension): File =
    new DiskFile(JFiles.createFile(underlying.resolve(name + extension.withDot)))

  /** Creates a container in this container with the given name. */
  protected override def createContainer(name: String): FileContainer =
    new DiskDirectory(JFiles.createDirectory(underlying.resolve(name).normalize()))

  override def hashCode(): Int =
    underlying.hashCode()

  override def equals(obj: Any): Boolean = obj match
    case otherDiskDirectory: DiskDirectory => underlying.equals(otherDiskDirectory.underlying)
    case _ => false