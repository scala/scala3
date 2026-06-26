/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package dotty.tools.io

import java.io.{ IOException, InputStream, OutputStream, FilterInputStream }
import java.nio.file.Files
import java.util.zip.{ ZipEntry, ZipFile }
import java.util.jar.JarFile
import scala.collection.mutable

/** An abstraction for zip files and streams.  Everything is written the way
 *  it is for performance: we come through here a lot on every run.  Be careful
 *  about changing it.
 *
 *  @author  Philippe Altherr (original version)
 *  @author  Paul Phillips (this one)
 *  @version 2.0,
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object ZipArchive {
  private[io] val closeZipFile: Boolean = sys.props.get("scala.classpath.closeZip").exists(_.toBoolean)

  private def dirName(path: String)  = splitPath(path, front = true)
  private def baseName(path: String) = splitPath(path, front = false)
  private def splitPath(path0: String, front: Boolean): String = {
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path  = if (isDir) path0.substring(0, path0.length - 1) else path0
    val idx   = path.lastIndexOf('/')

    if (idx < 0)
      if (front) "/"
      else path
    else
      if (front) path.substring(0, idx + 1)
      else path.substring(idx + 1)
  }
}
import ZipArchive.*
/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
abstract class ZipArchive(override val jpath: JPath) extends AbstractFile with Equals {
  self =>

  override def underlyingSource: Option[ZipArchive] = Some(this)
  override def isDirectory: Boolean = true
  override def lookupName(name: String, directory: Boolean): AbstractFile = unsupported()
  override def output: OutputStream    = unsupported()
  override def container: AbstractFile = unsupported()
  override def absolute: AbstractFile  = unsupported()

  /** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
  sealed abstract class Entry(path: String, val parent: Entry | Null) extends VirtualFile(baseName(path), path) {
    // have to keep this name for compat with sbt's compiler-interface
    def getArchive: ZipFile | Null = null
    override def underlyingSource: Option[ZipArchive] = Some(self)
    override def container: AbstractFile = if parent == null then NoAbstractFile else parent
    override def toString: String = self.path + "(" + path + ")"
  }

  /** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
  class DirEntry(path: String, parent: Entry | Null) extends Entry(path, parent) {
    val entries: mutable.HashMap[String, Entry] = mutable.HashMap()

    override def isDirectory: Boolean = true
    override def iterator: Iterator[Entry] = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry | Null = {
      if (directory) entries.get(name + "/").orNull
      else entries.get(name).orNull
    }
  }

  private def ensureDir(dirs: mutable.Map[String, DirEntry], path: String): DirEntry =
    //OPT inlined from getOrElseUpdate; saves ~50K closures on test run.
    // was:
    // dirs.getOrElseUpdate(path, {
    //   val parent = ensureDir(dirs, dirName(path), null)
    //   val dir    = new DirEntry(path)
    //   parent.entries(baseName(path)) = dir
    //   dir
    // })
    dirs get path match {
      case Some(v) => v
      case None    =>
        val parent = ensureDir(dirs, dirName(path))
        val dir    = new DirEntry(path, parent)
        parent.entries(baseName(path)) = dir
        dirs(path) = dir
        dir
    }

  protected def getDir(dirs: mutable.Map[String, DirEntry], entry: ZipEntry): DirEntry = {
    if (entry.isDirectory) ensureDir(dirs, entry.getName)
    else ensureDir(dirs, dirName(entry.getName))
  }

  def close(): Unit
}
/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
final class FileZipArchive(jpath: JPath, release: Option[String] = None) extends ZipArchive(jpath) {
  private def openZipFile(): ZipFile = try {
    release match {
      case Some(r) if file.nn.getName.endsWith(".jar") =>
        new JarFile(file, true, ZipFile.OPEN_READ, if r == "" then Runtime.version() else Runtime.Version.parse(r))
      case _ =>
        new ZipFile(file)
    }
  } catch {
    case ioe: IOException => throw new IOException("Error accessing " + file.nn.getPath, ioe)
  }

  private class LazyEntry(
    name: String,
    time: Long,
    size: Int,
    parent: DirEntry
  ) extends Entry(name, parent) {
    override def lastModified: Long = time // could be stale
    override def input: InputStream = {
      val zipFile  = openZipFile()
      val entry = zipFile.getEntry(name) // with `-release`, returns the correct version under META-INF/versions
      val `delegate` = zipFile.getInputStream(entry)
      new FilterInputStream(`delegate`) {
        override def close(): Unit = { zipFile.close() }
      }
    }
    override def sizeOption: Option[Int] = Some(size) // could be stale
  }

  // keeps a file handle open to ZipFile, which forbids file mutation
  // on Windows, and leaks memory on all OS (typically by stopping
  // classloaders from being garbage collected). But is slightly
  // faster than LazyEntry.
  private class LeakyEntry(
    zipFile: ZipFile,
    zipEntry: ZipEntry,
    parent: DirEntry
  ) extends Entry(zipEntry.getName, parent) {
    override def lastModified: Long = zipEntry.getTime
    override def input: InputStream = zipFile.getInputStream(zipEntry)
    override def sizeOption: Option[Int] = Some(zipEntry.getSize.toInt)
  }

  lazy val (root: DirEntry, allDirs: collection.Map[String, DirEntry]) = {
    val root = new DirEntry("/", null)
    val dirs = mutable.HashMap[String, DirEntry]("/" -> root)
    val zipFile = openZipFile()
    val entries = zipFile.entries()

    try {
      while (entries.hasMoreElements) {
        val zipEntry = entries.nextElement
        if (!zipEntry.getName.startsWith("META-INF/versions/")) {
          val zipEntryVersioned = if (release.isDefined) {
            // JARFile will return the entry for the corresponding release-dependent version here under META-INF/versions
            zipFile.getEntry(zipEntry.getName)
          } else zipEntry

          if (!zipEntry.isDirectory) {
            val dir = getDir(dirs, zipEntry)
            val f =
              if (ZipArchive.closeZipFile)
                new LazyEntry(
                  zipEntry.getName(),
                  zipEntry.getTime(),
                  zipEntry.getSize().toInt,
                  dir
                )
              else
                new LeakyEntry(zipFile, zipEntryVersioned, dir)

            dir.entries(f.name) = f
          }
        }
      }
    } finally {
      if (ZipArchive.closeZipFile) zipFile.close()
      else closeables ::= zipFile
    }
    (root, dirs)
  }

  override def iterator: Iterator[Entry] = root.iterator

  override def name: String       = jpath.getFileName.toString
  override def path: String       = jpath.toString
  override def input: InputStream = Files.newInputStream(jpath)
  override def lastModified: Long = Files.getLastModifiedTime(jpath).toMillis

  override def sizeOption: Option[Int] = Some(Files.size(jpath).toInt)
  override def canEqual(other: Any): Boolean = other.isInstanceOf[FileZipArchive]
  override def hashCode(): Int = jpath.hashCode
  override def equals(that: Any): Boolean = that match {
    case x: FileZipArchive =>
      jpath.toAbsolutePath == x.jpath.toAbsolutePath
    case _ =>
      false
  }

  private var closeables: List[java.io.Closeable] = Nil
  override def close(): Unit = {
    closeables.foreach(_.close)
    closeables = Nil
  }
}