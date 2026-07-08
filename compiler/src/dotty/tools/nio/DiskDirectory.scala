package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, SimpleFileVisitor, Files as JFiles, Path as JPath}
import scala.jdk.CollectionConverters.*

private[nio] object DiskDirectory:
  def getOrCreate(path: String): FileContainer =
    val jpath = JPath.of(path)
    if !JFiles.exists(jpath) then
      JFiles.createDirectory(jpath)
    new DiskDirectory(jpath)

private[nio] final class DiskDirectory(underlying: JPath) extends FileContainer:
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
    JFiles.walkFileTree(underlying, new SimpleFileVisitor[JPath]() {
      override def visitFile(file: JPath, attrs: BasicFileAttributes): FileVisitResult =
        JFiles.delete(file)
        FileVisitResult.CONTINUE
      override def postVisitDirectory(dir: JPath, exc: IOException): FileVisitResult =
        JFiles.delete(dir)
        FileVisitResult.CONTINUE
    })

  /** Gets the file in this container with the given name and extension if it exists. */
  protected override def getFile(name: String, extension: FileExtension): Option[File] =
    val res = underlying.resolve(name)
    if JFiles.isRegularFile(res) then Some(new DiskFile(res)) else None

  /** Gets the container in this container with the given name if it exists. */
  protected override def getContainer(name: String): Option[FileContainer] =
    val res = underlying.resolve(name)
    if JFiles.isDirectory(res) then Some(new DiskDirectory(res)) else None

  /** Creates a file in this container with the given name and extension. */
  protected override def createFile(name: String, extension: FileExtension): File =
    new DiskFile(JFiles.createFile(underlying.resolve(name)))

  /** Creates a container in this container with the given name. */
  protected override def createContainer(name: String): FileContainer =
    new DiskDirectory(JFiles.createDirectory(underlying.resolve(name)))