package dotty.tools.nio

import dotty.tools.io.FileExtension

import scala.collection.mutable

private[nio] final class MemoryContainer(realParent: Option[MemoryContainer], override val name: String) extends FileContainer:
  private val files: mutable.Map[String, MemoryFile] = mutable.Map.empty
  private val containers: mutable.Map[String, MemoryContainer] = mutable.Map.empty

  override def path: String =
    parent.path + FileSystemEntry.separator + name

  override def parent: FileContainer =
    realParent.getOrElse(this)

  override def enclosing: Option[File] =
    None

  override def entries: Iterable[FileSystemEntry] =
    files.values ++ containers.values

  override def deleteRecursively(): Unit =
    files.clear()
    containers.clear()

  protected override def getFile(name: String, extension: FileExtension): Option[File] =
    files.get(name + extension.withDot)

  protected override def getContainer(name: String): Option[FileContainer] =
    containers.get(name)

  protected override def createFile(name: String, extension: FileExtension): File =
    val res = new MemoryFile(this, name, extension)
    files(name + extension.withDot) = res
    res

  protected override def createContainer(name: String): FileContainer =
    val res = new MemoryContainer(Some(this), name)
    containers(name) = res
    res
