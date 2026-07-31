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

  /** Indicates whether `getFromFile` would succeed for the given file. */
  def canGetFromFile(file: File): Boolean =
    file.extension == FileExtension.Jar || file.extension == FileExtension.Zip

  /** If the given file is also a container, such as an archive, opens it as such and returns it, using the given JAR version if necessary. Otherwise, returns None. */
  def getFromFile(file: File, jarVersion: String, compressionLevel: Int, manifest: Map[String, String] = Map.empty): Option[FileContainer] =
    Option.when(canGetFromFile(file))(ZipContainer.open(file, jarVersion, compressionLevel, manifest))

  /** Creates a temporary file container on disk. */
  def createTemporaryOnDisk(nameHint: String): FileContainer =
    DiskDirectory.createTemporary(nameHint)

  /** Creates a new in-memory file container with the given name. Does not conflict with other in-memory containers with the same name. */
  def createInMemory(name: String): FileContainer =
    MemoryContainer.create(name)

  /** Utility for implementers. Gets the real path given path+ext+separator, avoiding unnecessary work and allocations if possible. */
  private[nio] def getPath(path: String, extension: FileExtension, separator: Char): String =
    if separator == FileSystemEntry.separator then
      if extension == FileExtension.Empty then path
      else path + extension.withDot
    else
      val replaced = path.replace(separator, FileSystemEntry.separator)
      if extension == FileExtension.Empty then replaced
      else replaced + extension.withDot

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
  def getFile(path: String, extension: FileExtension = FileExtension.Empty, separator: Char = FileSystemEntry.separator): Option[File]
  /** Gets the container in this container at the given path if it exists, optionally using the given path separator. */
  def getContainer(path: String, separator: Char = FileSystemEntry.separator): Option[FileContainer]

  /** Gets or creates a file in this container at the given path, optionally with the given extension and using the given path separator. */
  def getOrCreateFile(path: String, extension: FileExtension = FileExtension.Empty, separator: Char = FileSystemEntry.separator): File
  /** Gets or creates a container in this container at the given path, optionally using the given path separator. */
  def getOrCreateContainer(path: String, separator: Char = FileSystemEntry.separator): FileContainer

  /** Copies all contents of this container to the given container, overwriting existing entries if necessary. */
  def copyRecursivelyTo(other: FileContainer): Unit =
    entries.foreach {
      case f: File =>
        val newFile = other.getOrCreateFile(f.name)
        f.copyTo(newFile)
      case c: FileContainer =>
        val newContainer = other.getOrCreateContainer(c.name)
        c.copyRecursivelyTo(newContainer)
    }

  /** Deletes this file container and its contents. */
  def deleteRecursively(): Unit

  /** Closes this file container if necessary, and returns true if it was. */
  def close(): Boolean =
    false
