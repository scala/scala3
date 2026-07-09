package dotty.tools.nio

object FileSystemEntry:
  /** Separator between names in paths. */
  val separator: Char = java.io.File.separatorChar

abstract class FileSystemEntry:
  /** Name of this entry, including any extension and separating period if it's a file. */
  def name: String

  /** Full path of this entry. If the entry exists on disk, this path is usable as such. */
  def path: String

  /** Parent of this entry. The root container has itself as parent. */
  def parent: FileContainer

  /** If this entry is enclosed in an archive, returns that archive. Otherwise, returns None. */
  def enclosing: Option[File]

  /** Describes this file. Deliberately not `path` to catch misuse. */
  final override def toString: String =
    s"<file system entry at $path>"