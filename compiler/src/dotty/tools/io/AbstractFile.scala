/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools.io

import java.io.{
  IOException, InputStream, OutputStream, BufferedOutputStream,
  ByteArrayOutputStream
}
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files, Paths}

/**
 * An abstraction over files for use in the reflection/compiler libraries.
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 *
 * @author Philippe Altherr
 * @version 1.0, 23/03/2004
 */
object AbstractFile {
  def getFile(path: String): AbstractFile | Null = getFile(File(path))
  def getDirectory(path: String, jarVersion: String): AbstractFile | Null = getDirectory(Directory(path), jarVersion)
  def getFile(path: JPath): AbstractFile | Null = getFile(File(path))
  def getDirectory(path: JPath, jarVersion: String): AbstractFile | Null = getDirectory(Directory(path), jarVersion)

  /**
   * If the specified File exists and is a regular file, returns an
   * abstract regular file backed by it. Otherwise, returns `null`.
   */
  private def getFile(path: Path): AbstractFile | Null =
    if (path.isFile) new PlainFile(path) else null

  /**
   * If the specified File exists and is either a directory or a
   * readable zip or jar archive, returns an abstract directory
   * backed by it. Otherwise, returns `null`.
   */
  private def getDirectory(path: Path, jarVersion: String): AbstractFile | Null =
    if (path.isDirectory) new PlainFile(path)
    else if (path.isFile && path.ext.isJarOrZip) new FileZipArchive(path.jpath, Some(jarVersion))
    else null
}

/**
 * <p>
 *   This class and its children serve to unify handling of files and
 *   directories. These files and directories may or may not have some
 *   real counterpart within the file system. For example, some file
 *   handles reference files within a zip archive or virtual ones
 *   that exist only in memory.
 * </p>
 * <p>
 *   Every abstract file has a path (i.e. a full name) and a name
 *   (i.e. a short name) and may be backed by some real File. There are
 *   two different kinds of abstract files: regular files and
 *   directories. Regular files may be read and have a last modification
 *   time. Directories may list their content and look for subfiles with
 *   a specified name or path and of a specified kind.
 * </p>
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
abstract class AbstractFile extends dotty.tools.dotc.interfaces.AbstractFile {

  /** Returns the name of this abstract file. */
  def name: String

  /** Returns the path of this abstract file. */
  def path: String

  /** Returns the extension of this abstract file. */
  def ext: FileExtension = Path.fileExtension(name)

  /** Returns the containing directory of this abstract file, if any */
  def container: Option[AbstractFile] = None

  /** Returns the underlying File if any and null otherwise. */
  def file: JFile | Null = try {
    val jpath = this.jpath
    if (jpath == null) null
    else jpath.toFile
  } catch {
    case _: UnsupportedOperationException => null
  }

  /** Adapts `file` to the `dotty.tools.dotc.interfaces.AbstractFile` interface */
  def jfile: java.util.Optional[JFile] =
    java.util.Optional.ofNullable(file)

  /** Returns the underlying Path if any and null otherwise. */
  def jpath: JPath | Null

  /** Does this abstract file denote an existing file? */
  def exists: Boolean = {
    (jpath eq null) || Files.exists(jpath)
  }

  /** Is this abstract file a directory? */
  def isDirectory: Boolean

  /** Does this abstract file correspond to something on-disk? */
  def isVirtual: Boolean = false

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long

  /** returns an input stream so the file can be read */
  def input: InputStream

  /** Returns an output stream for writing the file */
  def output: OutputStream

  /** URL of the file if available. */
  def toURL: Option[URL]

  /** Returns contents of file (if applicable) in a Char array.
   *  warning: use `Global.getSourceFile()` to use the proper
   *  encoding when converting to the char array.
   */
  @throws(classOf[IOException])
  final def toCharArray: Array[Char] = new String(toByteArray).toCharArray

  /** Returns contents of file (if applicable) in a byte array. */
  @throws(classOf[IOException])
  final def toByteArray: Array[Byte] =
    val is = input
    try is.readAllBytes()
    finally is.close()

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile]

  /** Returns all subfiles of all subdirectories of this abstract directory, including itself. */
  final def deepIterator: Iterator[AbstractFile] =
    if isDirectory then iterator.flatMap(_.deepIterator)
    else Iterator.single(this)

  /**
   * Splits the given path using the given separator char, and finds the corresponding file through subdirectories.
   * Optionally adds the given suffix to the last component.
   * This is intended to make it easy to find files in formats such as "java/lang/Object" or "java.lang.Object".
   */
  final def lookupPath(path: String, separator: Char, lastSuffix: String = "", directory: Boolean = false): Option[AbstractFile] =
    var file: AbstractFile = this
    var idx = 0
    var nextStepIdx = -1
    while
      nextStepIdx = path.indexOf(separator, idx)
      nextStepIdx != -1
    do
      file.lookupName(path.substring(idx, nextStepIdx), directory = true) match
        case null => return None
        case f =>
          file = f
          idx = nextStepIdx + 1
    Option(file.lookupName(path.substring(idx) + lastSuffix, directory = directory))
  end lookupPath

  /** Returns the abstract file in this abstract directory with the specified
   *  name. If there is no such file, returns `null`. The argument
   *  `directory` tells whether to look for a directory or
   *  a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile | Null

  /** Returns the sibling abstract file in the parent of this abstract file or directory.
   *  If there is no such file, returns `null`.
   */
  final def resolveSibling(name: String): AbstractFile | Null =
    container.map(_.lookupName(name, directory = false)).orNull

  final def resolveSiblingWithExtension(extension: FileExtension): AbstractFile | Null =
    resolveSibling(Path.fileName(name) + "." + extension)

  private def fileOrSubdirectoryNamed(name: String, isDir: Boolean): AbstractFile =
    lookupName(name, isDir) match {
      case null =>
        val jpath = this.jpath.nn
        // the optional exception may be thrown for symlinks, notably /tmp on macOS.
        // isDirectory tests for existing directory. The default behavior is hypothetical isDirectory(jpath, FOLLOW_LINKS).
        try Files.createDirectories(jpath)
        catch { case _: FileAlreadyExistsException if Files.isDirectory(jpath) => }

        // a race condition in creating the entry after the failed lookup may throw
        val path = jpath.resolve(name)
        try
          if (isDir) Files.createDirectory(path)
          else Files.createFile(path)
        catch case _: FileAlreadyExistsException => ()
        new PlainFile(new File(path))
      case lookup => lookup
    }

  /**
   * Get the file in this directory with the given name,
   * creating an empty file if it does not already exist.
   */
  def fileNamed(name: String): AbstractFile = {
    assert(isDirectory, "Tried to find '%s' in '%s' but it is not a directory".format(name, path))
    fileOrSubdirectoryNamed(name, isDir = false)
  }

  /**
   * Get the subdirectory with a given name, creating it if it
   * does not already exist.
   */
  def subdirectoryNamed(name: String): AbstractFile = {
    assert (isDirectory, "Tried to find '%s' in '%s' but it is not a directory".format(name, path))
    fileOrSubdirectoryNamed(name, isDir = true)
  }

  protected def unsupported(): Nothing = unsupported(null)
  protected def unsupported(msg: String | Null): Nothing = throw new UnsupportedOperationException(msg)

  /** Returns the path of this abstract file. */
  override def toString(): String = path

}
