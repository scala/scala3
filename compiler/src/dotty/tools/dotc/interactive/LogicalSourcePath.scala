package dotty.tools.dotc.interactive

import dotty.tools.dotc.classpath.ClassFileEntry
import dotty.tools.dotc.classpath.ClassPathEntries
import dotty.tools.dotc.classpath.PackageEntry
import dotty.tools.dotc.classpath.PackageEntryImpl
import dotty.tools.dotc.classpath.PackageName
import dotty.tools.dotc.classpath.SourceFileEntry
import dotty.tools.dotc.classpath.SourceFileEntryImpl
import dotty.tools.io.AbstractFile
import dotty.tools.io.ClassPath

import java.io.File
import java.net.URL
import java.nio.file.Path

/**
 * A ClassPath implementation that can find sources regardless of the directory where they're declared.
 */
class LogicalSourcePath(val sourcepath: String, rootPackage: LogicalPackage)
    extends ClassPath {

  override def findClassFile(className: String): Option[AbstractFile] = None
  override def classes(inPackage: PackageName): Seq[ClassFileEntry] = Seq.empty

  override def hasPackage(inPackage: PackageName): Boolean =
    findPackage(
      inPackage.dottedString
    ).isDefined

  /** Return all packages contained inside `inPackage`. Package entries contain the *full name* of the package. */
  override def packages(inPackage: PackageName): Seq[PackageEntry] =
    val rawPackage = inPackage.dottedString
    findPackage(rawPackage) match
      case Some(pkg) => packagesIn(pkg, rawPackage)
      case None => Seq.empty[PackageEntry]


  /** Return all sources contained directly inside `inPackage` */
  override def sources(inPackage: PackageName): Seq[SourceFileEntry] =
    val rawPackage = inPackage.dottedString
    findPackage(rawPackage) match
      case Some(pkg) =>
        sourcesIn(pkg)
      case None => Seq.empty[SourceFileEntry]

  private def sourcesIn(pkg: LogicalPackage) =
    pkg.sources.map(p => SourceFileEntryImpl(p))

  private def packagesIn(pkg: LogicalPackage, prefix: String) =
    val pre = if (prefix.isEmpty) prefix else s"$prefix."
    pkg.packages.map(p => PackageEntryImpl(pre + p.name))

  override def list(inPackage: PackageName): ClassPathEntries =
    val rawPackage = inPackage.dottedString
    val res = findPackage(rawPackage) match
      case Some(pkg) =>
        ClassPathEntries(packagesIn(pkg, rawPackage), sourcesIn(pkg))
      case None => ClassPathEntries(Seq(), Seq())
    res

  override def asURLs: Seq[URL] = sourcepath.split(File.pathSeparator).nn.toIndexedSeq.nn.map(new File(_)).nn.map(_.toURI.nn.toURL.nn)

  override def asClassPathStrings: Seq[String] = Seq()

  override def asSourcePathString: String = sourcepath

  /** Return the package for the given fullName, if any */
  private def findPackage(fullName: String): Option[LogicalPackage] =
    if fullName == "" then Option(rootPackage)
    else
      fullName.split('.').foldLeft(Option(rootPackage)) { (pkg, name) =>
        pkg.flatMap(_.getPackage(name))
      }

  override def toString: String = rootPackage.prettyPrint()

}

