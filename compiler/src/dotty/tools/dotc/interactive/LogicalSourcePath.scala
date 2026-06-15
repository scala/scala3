package dotty.tools.dotc.interactive

import dotty.tools.dotc.classpath.PackageEntry
import dotty.tools.dotc.classpath.SourceFileEntry
import dotty.tools.io.ClassPath

import java.io.File
import java.net.URL

/**
 * A ClassPath implementation that can find sources regardless of the directory where they're declared.
 */
class LogicalSourcePath(val sourcepath: String, rootPackage: LogicalPackage)
    extends ClassPath {

  override def hasPackage(inPackage: String): Boolean =
    findPackage(inPackage).isDefined

  /** Return all packages contained inside `inPackage`. Package entries contain the *full name* of the package. */
  override def packages(inPackage: String): Seq[PackageEntry] =
    findPackage(inPackage) match
      case Some(pkg) => packagesIn(pkg, inPackage)
      case None => Seq.empty[PackageEntry]


  /** Return all sources contained directly inside `inPackage` */
  override def sources(inPackage: String): Seq[SourceFileEntry] =
    findPackage(inPackage) match
      case Some(pkg) =>
        sourcesIn(pkg)
      case None => Seq.empty[SourceFileEntry]

  private def sourcesIn(pkg: LogicalPackage) =
    pkg.sources.map(p => SourceFileEntry(p))

  private def packagesIn(pkg: LogicalPackage, prefix: String) =
    val pre = if (prefix.isEmpty) prefix else s"$prefix."
    pkg.packages.map(p => PackageEntry(pre + p.name))

  override def asURLs: Seq[URL] = sourcepath.split(File.pathSeparator).toIndexedSeq.map(new File(_)).map(_.toURI.toURL)

  /** Return the package for the given fullName, if any */
  private def findPackage(fullName: String): Option[LogicalPackage] =
    if fullName == "" then Option(rootPackage)
    else
      fullName.split('.').foldLeft(Option(rootPackage)) { (pkg, name) =>
        pkg.flatMap(_.getPackage(name))
      }

  override def toString: String = rootPackage.prettyPrint()

}

