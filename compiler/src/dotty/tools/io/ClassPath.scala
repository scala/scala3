/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package dotty.tools
package io

import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.util.regex.PatternSyntaxException
import File.pathSeparator
import Jar.isJarOrZip
import dotc.classpath.{BinaryFileEntry, PackageEntry, SourceFileEntry}

/**
  * A representation of the compiler's class- or sourcepath.
  */
trait ClassPath {
  def asURLs: Seq[URL] = Seq.empty
  def hasPackage(pkg: String): Boolean = false
  def packages(inPackage: String): Seq[PackageEntry] = Seq.empty
  def classes(inPackage: String): Seq[BinaryFileEntry] = Seq.empty
  def sources(inPackage: String): Seq[SourceFileEntry] = Seq.empty

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
  private def expandS(pattern: String): Vector[String] = {
    val wildSuffix = File.separator + "*"

    /* Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list.filter(x => filt(x.name) && (x.isDirectory || isJarOrZip(x))).map(_.path).toVector

    if (pattern == "*") lsDir(Directory("."))
    // On Windows the JDK supports forward slash or backslash in classpath entries
    else if (pattern.endsWith(wildSuffix) || pattern.endsWith("/*")) lsDir(Directory(pattern dropRight 2))
    else if (pattern.contains('*')) {
      try {
        val regexp = ("^" + pattern.replace("""\*""", """.*""") + "$").r
        lsDir(Directory(pattern).parent, regexp.findFirstIn(_).isDefined)
      }
      catch { case _: PatternSyntaxException => Vector(pattern) }
    }
    else Vector(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): Vector[String] = path.split(pathSeparator).toVector.filterNot(_ == "").distinct

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): Vector[String] =
    if (expandStar) split(path).flatMap(expandS)
    else split(path)

  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): Vector[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Vector()

    val baseDir = file.parent
    new Jar(file).classPathElements map (elem =>
      specToURL(elem, baseDir) getOrElse (baseDir / elem).toURL
    )
  }

  private def specToURL(spec: String, basedir: Directory): Option[URL] =
    try
      val uri = new URI(spec)
      if uri.isAbsolute() then Some(uri.toURL())
      else
        Some(basedir.resolve(Path(spec)).toURL)
    catch
      case _: MalformedURLException | _: URISyntaxException => None
}

trait ClassRepresentation {
  def fileName: String
  def name: String
  def binary: Option[AbstractFile]
  def source: Option[AbstractFile]

  /** returns the length of `name` by stripping the extension of `fileName`
   *
   *  Used to avoid creating String instance of `name`.
   */
  final def nameLength: Int = {
    val ix = fileName.lastIndexOf('.')
    if (ix < 0) fileName.length else ix
  }
}
