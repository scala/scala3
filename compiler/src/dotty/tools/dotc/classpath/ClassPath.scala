/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.dotc
import dotty.tools.io.File.pathSeparator
import dotty.tools.io.{AbstractFile, Directory, File, FileExtension}

import java.net.URL
import java.util.regex.PatternSyntaxException

import scala.collection.mutable.ArrayBuffer

/**
 * A representation of the compiler's class- or sourcepath.
 */
trait ClassPath {
  def asURLs: Iterable[URL] = Seq.empty
  def hasPackage(pkg: String): Boolean = false
  def packages(inPackage: String): Iterable[String] = Seq.empty
  def classes(inPackage: String): Iterable[BinaryFileEntry] = Seq.empty
  def sources(inPackage: String): Iterable[SourceFileEntry] = Seq.empty

  /**
   * Returns *only* the classfile for an external name, e.g., "java.lang.String". This method does not
   * return source files or tasty files.
   *
   * This method is used by the classfile parser. When parsing a Java class, its own inner classes
   * are entered with a `ClassfileLoader` that parses the classfile returned by this method.
   * It is also used in the backend, by the inliner, to obtain the bytecode when inlining from the
   * classpath. It's also used by scalap.
   */
  def findClassFile(className: String): Option[AbstractFile] = None
}

object ClassPath {
  val RootPackage: String = ""

  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /* Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list.filter(x => filt(x.name) && (x.isDirectory || x.ext.isJarOrZip)).map(_.path).toList

    if (pattern == "*") lsDir(Directory("."))
    // On Windows the JDK supports forward slash or backslash in classpath entries
    else if (pattern.endsWith(wildSuffix) || pattern.endsWith("/*")) lsDir(Directory(pattern dropRight 2))
    else if (pattern.contains('*')) {
      try {
        val regexp = ("^" + pattern.replace("""\*""", """.*""") + "$").r
        lsDir(Directory(pattern).parent, regexp.findFirstIn(_).isDefined)
      }
      catch { case _: PatternSyntaxException => List(pattern) }
    }
    else List(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): List[String] = path.split(pathSeparator).toList.filterNot(_ == "").distinct

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path).flatMap(expandS)
    else split(path)

  /** Pair classfile/TASTy entries with source entries of the same name.
   *
   *  `classes` and `sources` are listed independently on the classpath. Without
   *  this merge, a class that exists both as TASTy and as a `.scala` file on
   *  `-sourcepath` would be entered twice: once from TASTy and once via
   *  `enterToplevelsFromSource`. `SymbolLoaders.initializeFromClassPath` then
   *  never sees `(binary, source)` together and cannot apply `needCompile`.
   */
  private[dotty] def mergeClassesAndSources(
      classes: Iterable[BinaryFileEntry],
      sources: Iterable[SourceFileEntry],
  ): Seq[ClassRepresentation] =
    val indices = dotc.util.HashMap[String, Int]()
    val merged = new ArrayBuffer[ClassRepresentation](classes.size + sources.size)
    var count = 0
    for entry <- classes do
      if !indices.contains(entry.name) then
        indices(entry.name) = count
        merged += entry
        count += 1
    for entry <- sources do
      indices.get(entry.name) match
        case Some(index) =>
          merged(index) match
            case binary: BinaryFileEntry =>
              merged(index) = BinaryAndSourceFilesEntry(binary, entry)
            case _ =>
        case None =>
          indices(entry.name) = count
          merged += entry
          count += 1
    if merged.isEmpty then Nil else merged.toSeq
}

trait ClassRepresentation {
  def fileName: String
  def name: String
  def binary: Option[AbstractFile]
  def source: Option[AbstractFile]
}

/** A TASTy file or classfile */
private[dotty] final case class BinaryFileEntry(file: AbstractFile) extends ClassRepresentation {
  def fileName: String = file.name
  def name: String = FileUtils.stripExtension(file.name) // class name
  def binary: Option[AbstractFile] = Some(file)
  def source: Option[AbstractFile] = None
}

private[dotty] final case class SourceFileEntry(file: AbstractFile) extends ClassRepresentation {
  def fileName: String = file.name
  def name: String = FileUtils.stripSourceExtension(file.name)
  def binary: Option[AbstractFile] = None
  def source: Option[AbstractFile] = Some(file)
}

/** A class that exists both as a classfile/TASTy and as a source file. */
private[dotty] final case class BinaryAndSourceFilesEntry(
    binaryEntry: BinaryFileEntry,
    sourceEntry: SourceFileEntry,
) extends ClassRepresentation {
  def fileName: String = binaryEntry.fileName
  def name: String = binaryEntry.name
  def binary: Option[AbstractFile] = binaryEntry.binary
  def source: Option[AbstractFile] = sourceEntry.source
}
