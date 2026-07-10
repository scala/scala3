package dotty.tools.nio

import dotty.tools.io.FileExtension

object FileContainer:
  /** Gets the current working directory on disk. */
  def workingDirectory(): FileContainer =
    DiskDirectory.workingDirectory()

  /** Gets a disk directory at the given path if it exists. */
  def getOnDisk(path: String): Option[FileContainer] =
    DiskDirectory.get(path)

  /** Gets or creates a disk directory at the given path on disk. */
  def getOrCreateOnDisk(path: String): FileContainer =
    DiskDirectory.getOrCreate(path)

  /** If the given file is also a container, such as an archive, opens it as such and returns it, using the given JAR version if necessary. Otherwise, returns None. */
  def getFromFile(file: File, jarVersion: String, compressionLevel: Int, manifest: Map[String, String] = Map.empty): Option[FileContainer] =
    if file.extension == FileExtension.Jar then Some(JarContainer.open(file, jarVersion, compressionLevel, manifest))
    else if file.extension == FileExtension.Zip then Some(ZipContainer.open(file))
    else None

  /** Creates a temporary file container on disk. */
  def createTemporaryOnDisk(nameHint: String): FileContainer =
    DiskDirectory.createTemporary(nameHint)

  /** Creates a new in-memory file container with the given name. Does not conflict with other in-memory containers with the same name. */
  def createInMemory(name: String): FileContainer =
    new MemoryContainer(None, name)

abstract class FileContainer extends FileSystemEntry:
  /** All file system entries directly contained by this container. */
  def entries: Iterable[FileSystemEntry]

  /** All file system entries recursively contained by this container, not including itself. */
  def recursiveEntries: Iterable[FileSystemEntry] =
    entries.flatMap {
      case c: FileContainer => Iterable.single(c) ++ c.recursiveEntries
      case e => Iterable.single(e)
    }

  /** Gets the file in this container at the given path if it exists, optionally with the given extension and using the given path separator. */
  final def getFile(path: String, extension: FileExtension = FileExtension.Empty, separator: Char = FileSystemEntry.separator): Option[File] =
    lookupPath(path, separator, create = false, isFile = true, extension = extension).map(_.asInstanceOf[File])
  /** Gets the container in this container at the given path if it exists, optionally using the given path separator. */
  final def getContainer(path: String, separator: Char = FileSystemEntry.separator): Option[FileContainer] =
    lookupPath(path, separator, create = false, isFile = false, extension = FileExtension.Empty).map(_.asInstanceOf[FileContainer])

  /** Gets or creates a file in this container at the given path, optionally with the given extension and using the given path separator. */
  def getOrCreateFile(path: String, extension: FileExtension = FileExtension.Empty, separator: Char = FileSystemEntry.separator): File =
    lookupPath(path, separator, create = true, isFile = true, extension = extension).get.asInstanceOf[File]
  /** Gets or creates a container in this container at the given path, optionally using the given path separator. */
  def getOrCreateContainer(path: String, separator: Char = FileSystemEntry.separator): FileContainer =
    lookupPath(path, separator, create = true, isFile = false, extension = FileExtension.Empty).get.asInstanceOf[FileContainer]

  /** Deletes this file container and its contents. */
  def deleteRecursively(): Unit

  /** Closes this file container if necessary, and returns true if it was. */
  def close(): Boolean =
    false

  /** Gets the file in this container with the given name and extension if it exists. */
  protected def getFile(name: String, extension: FileExtension): Option[File]
  /** Gets the container in this container with the given name if it exists. */
  protected def getContainer(name: String): Option[FileContainer]

  /** Creates a file in this container with the given name and extension. */
  protected def createFile(name: String, extension: FileExtension): File
  /** Creates a container in this container with the given name. */
  protected def createContainer(name: String): FileContainer

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