package dotty.tools.nio

import dotty.tools.io.FileExtension

private[nio] final class JarContainer(underlying: File, version: String) extends FileContainer:
  override def name: String =
    underlying.name

  override def path: String =
    underlying.path

  override def parent: FileContainer =
    underlying.parent

  override def enclosing: Option[File] =
    None

  override def entries: Iterable[FileSystemEntry] =
    ???

  override def deleteRecursively(): Unit =
    underlying.delete()

  protected override def getFile(name: String, extension: FileExtension): Option[File] =
    ???

  protected override def getContainer(name: String): Option[FileContainer] =
    ???

  protected override def createFile(name: String, extension: FileExtension): File =
    ???

  protected override def createContainer(name: String): FileContainer =
    ???

