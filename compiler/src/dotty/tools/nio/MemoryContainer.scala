package dotty.tools.nio

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

  override def getFile(path: String, extension: FileExtension | Null = null, separator: Char = FileSystemEntry.separator): Option[File] =
    lookupPath(path, extension, separator, create = false, isFile = true).map(_.asInstanceOf[File])

  override def getContainer(path: String, separator: Char = FileSystemEntry.separator): Option[FileContainer] =
    lookupPath(path, null, separator, create = false, isFile = false).map(_.asInstanceOf[FileContainer])

  override def getOrCreateFile(path: String, extension: FileExtension | Null = null, separator: Char = FileSystemEntry.separator): File =
    lookupPath(path, extension, separator, create = true, isFile = true).get.asInstanceOf[File]

  override def getOrCreateContainer(path: String, separator: Char = FileSystemEntry.separator): FileContainer =
    lookupPath(path, null, separator, create = true, isFile = false).get.asInstanceOf[FileContainer]

  override def deleteRecursively(): Unit =
    files.clear()
    containers.clear()
    realParent.foreach(_.containers.remove(name))


  private def createFile(name: String): MemoryFile =
    val res = new MemoryFile(this, name)
    files(name) = res
    res

  private def createContainer(name: String): MemoryContainer =
    val res = new MemoryContainer(Some(this), name)
    containers(name) = res
    res

  private def lookupPath(path: String, extension: FileExtension | Null, separator: Char, create: Boolean, isFile: Boolean): Option[FileSystemEntry] =
    var container = this
    var idx = 0
    var nextStepIdx = -1
    while
      nextStepIdx = path.indexOf(separator, idx)
      nextStepIdx != -1
    do
      val name = path.substring(idx, nextStepIdx)
      idx = nextStepIdx + 1
      container.containers.get(name) match
        case Some(c) =>
          container = c
        case None if create =>
          container = container.createContainer(name)
        case None =>
          return None
    val finalName = FileContainer.getPath(path.substring(idx), extension)
    if isFile then
      container.files.get(finalName) match
        case s @ Some(_) => s
        case None if create => Some(container.createFile(finalName))
        case None => None
    else
      if finalName == "" then Some(container)
      else container.containers.get(finalName) match
        case s @ Some(_) => s
        case None if create => Some(container.createContainer(finalName))
        case None => None