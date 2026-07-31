package dotty.tools.nio

import dotty.tools.io.FileExtension

import scala.collection.mutable

private object MemoryContainer:
  def create(name: String) =
    new MemoryContainer(None, name)

private final class MemoryContainer(realParent: Option[MemoryContainer], override val name: String) extends FileContainer:
  val files: mutable.Map[String, MemoryFile] = mutable.Map.empty
  private val containers: mutable.Map[String, MemoryContainer] = mutable.Map.empty

  override def path: String = realParent match
    case Some(p) => p.path + FileSystemEntry.separator + name
    case None => "memory:" + name

  override def parent: FileContainer =
    realParent.getOrElse(this)

  override def enclosing: Option[File] =
    None

  override def entries: Iterable[FileSystemEntry] =
    files.values ++ containers.values

  override def getFile(path: String, extension: FileExtension = FileExtension.Empty, separator: Char = FileSystemEntry.separator): Option[File] =
    lookupPath(path, separator, create = false, isFile = true, extension = extension).map(_.asInstanceOf[File])

  override def getContainer(path: String, separator: Char = FileSystemEntry.separator): Option[FileContainer] =
    lookupPath(path, separator, create = false, isFile = false, extension = FileExtension.Empty).map(_.asInstanceOf[FileContainer])

  override def getOrCreateFile(path: String, extension: FileExtension = FileExtension.Empty, separator: Char = FileSystemEntry.separator): File =
    lookupPath(path, separator, create = true, isFile = true, extension = extension).get.asInstanceOf[File]

  override def getOrCreateContainer(path: String, separator: Char = FileSystemEntry.separator): FileContainer =
    lookupPath(path, separator, create = true, isFile = false, extension = FileExtension.Empty).get.asInstanceOf[FileContainer]

  override def deleteRecursively(): Unit =
    files.clear()
    containers.clear()
    realParent.foreach(_.containers.remove(name))


  private def getFile(name: String, extension: FileExtension): Option[MemoryFile] =
    files.get(name + extension.withDot)

  private def getContainer(name: String): Option[MemoryContainer] =
    containers.get(name)

  private def createFile(name: String, extension: FileExtension): MemoryFile =
    val res = new MemoryFile(this, name, extension)
    files(name + extension.withDot) = res
    res

  private def createContainer(name: String): MemoryContainer =
    val res = new MemoryContainer(Some(this), name)
    containers(name) = res
    res

  private def lookupPath(path: String, separator: Char, create: Boolean, isFile: Boolean, extension: FileExtension): Option[FileSystemEntry] =
    var container = this
    var idx = 0
    var nextStepIdx = -1
    while
      nextStepIdx = path.indexOf(separator, idx)
      nextStepIdx != -1
    do
      val name = path.substring(idx, nextStepIdx)
      idx = nextStepIdx + 1
      container.getContainer(name) match
        case Some(c) =>
          container = c
        case None if create =>
          container = container.createContainer(name)
        case None =>
          return None
    val finalName = path.substring(idx)
    if isFile then
      container.getFile(finalName, extension) match
        case s @ Some(_) => s
        case None if create => Some(container.createFile(finalName, extension))
        case None => None
    else
      if finalName == "" then Some(container)
      else container.getContainer(finalName) match
        case s @ Some(_) => s
        case None if create => Some(container.createContainer(finalName))
        case None => None