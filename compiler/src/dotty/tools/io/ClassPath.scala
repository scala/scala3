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
  def asURLs: Seq[URL]
  def hasPackage(pkg: String): Boolean
  def packages(inPackage: String): Seq[PackageEntry]
  def classes(inPackage: String): Seq[BinaryFileEntry]
  def sources(inPackage: String): Seq[SourceFileEntry]

  /**
   * Returns *only* the classfile for an external name, e.g., "java.lang.String". This method does not
   * return source files or tasty files.
   *
   * This method is used by the classfile parser. When parsing a Java class, its own inner classes
   * are entered with a `ClassfileLoader` that parses the classfile returned by this method.
   * It is also used in the backend, by the inliner, to obtain the bytecode when inlining from the
   * classpath. It's also used by scalap.
   */
  def findClassFile(className: String): Option[AbstractFile] =
    findClassFileAndModuleFile(className, findModule = false).map(_._1)

  /** Same as `findClassFile`, but also returns the corresponding module-info class file if there is any. */
  def findClassFileAndModuleFile(className: String): Option[(AbstractFile, Option[AbstractFile])] =
    findClassFileAndModuleFile(className, findModule = true)

  def findClassFileAndModuleFile(className: String, findModule: Boolean): Option[(AbstractFile, Option[AbstractFile])]
}

object ClassPath {
  val RootPackage: String = ""

  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /* Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list.filter(x => filt(x.name) && (x.isDirectory || isJarOrZip(x))).map(_.path).toList

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

  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): List[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Nil

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
