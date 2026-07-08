package dotty.tools.nio

object FileSystemEntry:
  /** Separator between file and folder names on disk. The separator for other kinds of paths is not defined. */
  val separator: Char = java.io.File.separatorChar

abstract class FileSystemEntry:
  /** Name of this entry, without extension nor separating dot if it's a file. */
  def name: String

  /** Full path of this entry. If the entry exists on disk, this path is usable as such. */
  def path: String

  /** Parent of this entry. The root container has itself as parent. */
  def parent: FileContainer

  /** If this entry is enclosed in an archive, returns that archive. Otherwise, returns None. */
  def enclosing: Option[File]
