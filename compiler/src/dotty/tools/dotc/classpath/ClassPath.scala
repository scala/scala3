package dotty.tools.dotc.classpath

import dotty.tools.dotc
import dotty.tools.io.File.pathSeparator
import dotty.tools.io.{AbstractFile, Directory, File, FileExtension}

import java.util.regex.PatternSyntaxException
import java.nio.file.*
import java.net.URI

import scala.collection.mutable
import scala.util.Properties

/**
 * A representation of the compiler's class- or sourcepath.
 */
trait ClassPath {
  def hasPackage(pkg: String): Boolean = false
  def packages(inPackage: String): Iterable[String] = Seq.empty
  def classes(inPackage: String): Iterable[BinaryFileEntry] = Seq.empty
  def sources(inPackage: String): Iterable[SourceFileEntry] = Seq.empty

  /** Which locations are searched by this classpath. Used to create a class loader from it. */
  def searchLocations: Iterable[AbstractFile]

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

  /** Aggregates the given classpaths. */
  def aggregate(classPaths: List[ClassPath]): ClassPath =
    AggregateClassPath(classPaths)

  /** Attempts to create a classpath for the Java libraries of the given Java release. */
  def jrt(release: Option[String]): Option[ClassPath] =
    // Longer term we'd like an official API for this in the JDK
    // Discussion: http://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/thread.html#11738
    release match
      case Some(v) if v.toInt < Runtime.version().feature() =>
        try
          val ctSym = Paths.get(Properties.javaHome).resolve("lib").resolve("ct.sym")
          if (Files.notExists(ctSym)) None
          else Some(new CtSymClassPath(ctSym, v.toInt))
        catch case _: Exception => None
      case _ =>
        try Some(new JrtClassPath(FileSystems.getFileSystem(URI.create("jrt:/"))))
        catch case _: ProviderNotFoundException | _: FileSystemNotFoundException => None

  /** Replaces ct.sym with jrt in the given classpath. */
  def replaceCtSymWithJrt(classPath: ClassPath): ClassPath =
    classPath match
      case AggregateClassPath((_: CtSymClassPath) :: rest) =>
        jrt(release = None) match
          case Some(jrt) => AggregateClassPath(jrt :: rest)
          case None => classPath
      case _ => classPath

  /** Update classpath with a substituted subentry */
  def replace(classPath: ClassPath, subst: Map[ClassPath, ClassPath]): ClassPath =
    classPath match
      case AggregateClassPath(entries) => AggregateClassPath(entries.map(e => subst.getOrElse(e, e)))
      case cp => subst.getOrElse(cp, cp)

  /** Adds the two given classpaths. */
  def add(classPath: ClassPath, newClassPath: ClassPath): ClassPath =
    classPath match
      case AggregateClassPath(entries) => AggregateClassPath(entries :+ newClassPath)
      case cp => AggregateClassPath(cp :: newClassPath :: Nil)

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

  /** Transforms a .class file name to a .tasty file name, for use in finding .tasty files. */
  def classNameToTasty(fileName: String): String =
    val classOrModuleName = fileName.stripSuffix(".class")
    val className =
      if classOrModuleName.endsWith("$")
        && classOrModuleName != "Null$" // scala.runtime.Null$
        && classOrModuleName != "Nothing$" // scala.runtime.Nothing$
        // Special case for `object $` in Ammonite.
        // This is an ad-hoc workaround for Ammonite `object $`. See issue #19702
        // This definition is not valid Scala.
        && classOrModuleName != "$"
      then classOrModuleName.stripSuffix("$")
      else classOrModuleName
    className + ".tasty"

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
  ): Iterable[ClassRepresentation] =
    if sources.isEmpty then return classes
    if classes.isEmpty then return sources

    val indices = mutable.Map.empty[String, Int]
    val merged = new mutable.ArrayBuffer[ClassRepresentation](classes.size + sources.size)
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
