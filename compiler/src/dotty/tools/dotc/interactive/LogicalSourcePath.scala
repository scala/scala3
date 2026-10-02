package dotty.tools.dotc.interactive

import dotty.tools.dotc.classpath.SourceFileEntry
import dotty.tools.dotc.classpath.ClassPath
import dotty.tools.io.*
import dotty.tools.io.PlainFile.toPlainFile

import java.io.File

/**
 * A ClassPath implementation that can find sources regardless of the directory where they're declared.
 */
class LogicalSourcePath(val sourcepath: String, rootPackage: LogicalPackage)
    extends ClassPath {

  override def hasPackage(inPackage: String): Boolean =
    findPackage(inPackage).isDefined

  /** Return all packages contained inside `inPackage`. Package entries contain the *full name* of the package. */
  override def packages(inPackage: String): Iterable[String] =
    findPackage(inPackage) match
      case Some(pkg) => packagesIn(pkg, inPackage)
      case None => Iterable.empty


  /** Return all sources contained directly inside `inPackage` */
  override def sources(inPackage: String): Iterable[SourceFileEntry] =
    findPackage(inPackage) match
      case Some(pkg) =>
        sourcesIn(pkg)
      case None => Iterable.empty

  private def sourcesIn(pkg: LogicalPackage) =
    pkg.sources.map(p => SourceFileEntry(p))

  private def packagesIn(pkg: LogicalPackage, prefix: String) =
    val pre = if (prefix.isEmpty) prefix else s"$prefix."
    pkg.packages.map(p => pre + p.name)

  override def searchDirectories: Iterable[AbstractFile] =
    sourcepath.split(File.pathSeparator).map(s => java.nio.file.Path.of(s).toPlainFile)

  /** Return the package for the given fullName, if any */
  private def findPackage(fullName: String): Option[LogicalPackage] =
    if fullName == "" then Option(rootPackage)
    else
      fullName.split('.').foldLeft(Option(rootPackage)) { (pkg, name) =>
        pkg.flatMap(_.getPackage(name))
      }

  override def toString: String = rootPackage.prettyPrint()

}

