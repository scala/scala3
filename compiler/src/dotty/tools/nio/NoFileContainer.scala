package dotty.tools.nio

import dotty.tools.io.FileExtension

import java.io.IOException

private[nio] object NoFileContainer extends FileContainer:
  override def name: String = "<no container>"
  override def path: String = name
  override def parent: FileContainer = this
  override def enclosing: Option[File] = None
  override def entries: Iterable[FileSystemEntry] = Iterable.empty
  override def deleteRecursively(): Unit = ()

  protected override def getFile(name: String, extension: FileExtension): Option[File] = None
  protected override def getContainer(name: String): Option[FileContainer] = None
  protected override def createFile(name: String, extension: FileExtension): File = throw new IOException("Cannot create files in NoFileContainer")
  protected override def createContainer(name: String): FileContainer = throw new IOException("Cannot create containers in NoFileContainer")
