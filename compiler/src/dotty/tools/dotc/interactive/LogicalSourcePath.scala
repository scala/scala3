// we need to be in this package to access package private classes
// like SourceFileEntryImpl and PackageEntryImpl
package dotty.tools.dotc.interactive

import java.io.File
import java.net.URL
import java.nio.file.Path
import java.{util => ju}
import dotty.tools.io.ClassPath
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.classpath.PackageName
import dotty.tools.dotc.classpath.BinaryFileEntry
import dotty.tools.dotc.classpath.SourceFileEntry
import dotty.tools.dotc.classpath.PackageEntry
import dotty.tools.dotc.classpath.ClassPathEntries
import dotty.tools.dotc.classpath.PackageEntryImpl

/**
 * A ClassPath implementation that can find sources regardless of the directory where they're declared.
 */
class LogicalSourcePath(val dirs: Seq[File], rootPackage: LogicalPackage)
    extends ClassPath {

  // We don't have any classfiles, only source files
  def findClassFile(className: String): Option[AbstractFile] = None
  override def classes(inPackage: PackageName): Seq[BinaryFileEntry] = Seq.empty

  override def hasPackage(inPackage: PackageName): Boolean = findPackage(
    inPackage.dottedString
  ).isDefined

  /** Return all packages contained inside `inPackage`. Package entries contain the *full name* of the package. */
  override def packages(inPackage: PackageName): Seq[PackageEntry] = {
    
    val rawPackage = inPackage.dottedString
    findPackage(rawPackage) match {
      case Some(pkg) =>
        val res = packagesIn(pkg, rawPackage)
        println(s"Packages in package: ${inPackage.dottedString} ${res}")
        res
      case None => Seq.empty[PackageEntry]
    }
  }

  /** Return all sources contained directly inside `inPackage` */
  override def sources(inPackage: PackageName): Seq[SourceFileEntry] = {
    val rawPackage = inPackage.dottedString
    findPackage(rawPackage) match {
      case Some(pkg) =>
        val res = sourcesIn(pkg)
        res
      case None => Seq.empty[SourceFileEntry]
    }
  }

  private def sourcesIn(pkg: LogicalPackage) = {
    pkg.sources.map(p => SourceFileEntry(AbstractFile.getFile(p)))
  }

  private def packagesIn(pkg: LogicalPackage, prefix: String) = {
    val pre = if (prefix.isEmpty) prefix else s"$prefix."
    pkg.packages.map(p => PackageEntryImpl(pre + p.name))
  }

  /** Allows to get entries for packages and classes merged with sources possibly in one pass. */
  override def list(inPackage: PackageName): ClassPathEntries = {
    val rawPackage = inPackage.dottedString
    val res = findPackage(rawPackage) match {
      case Some(pkg) =>
        println(s"Listing package: ${inPackage.dottedString} ${sourcesIn(pkg)}")
        ClassPathEntries(packagesIn(pkg, rawPackage), sourcesIn(pkg))
      case None => ClassPathEntries(Seq(), Seq())
    }
    res
  }

  /** Not sure what the purpose of this method really is, it's not called by the compiler */
  override def asURLs: Seq[URL] = dirs.map(_.toURI.toURL)

  override def asClassPathStrings: Seq[String] = Seq()

  override def asSourcePathString: String =
    dirs.map(_.toString).mkString(File.pathSeparator)

  /** Return the package for the given fullName, if any */
  private def findPackage(fullName: String): Option[LogicalPackage] = {
    if (fullName == "") Option(rootPackage)
    else
      fullName.split('.').foldLeft(Option(rootPackage)) { (pkg, name) =>
        pkg.flatMap(_.getPackage(name))
      }
  }

  override def toString: String = rootPackage.prettyPrint()
}

/**
 * A logical package representation. This is disconnected from the file system, and faithfully
 * represents the nesting of packages and sources that contribute classes to those packages.
 */
trait LogicalPackage {

  /** The name of the package, or the empty string if it's the root package */
  def name: String

  /** Return all member packages. Only direct members are returned. */
  def packages: Seq[LogicalPackage]

  /**
   * Return all sources contained by this package. Only direct members are returned, and there are no duplicates.
   *
   */
  def sources: Seq[String]

  /** Return this directly nested package, if it exists */
  def getPackage(name: String): Option[LogicalPackage]

  /**
   * Pretty print the package tree.
   */
  def prettyPrint(): String = {
    prettyPrintWith().toString().stripTrailing().stripIndent()
  }

  private def prettyPrintWith(
      indent: Int = 0,
      sb: StringBuilder = new StringBuilder
  ): StringBuilder = {
    sb ++= " " * indent
    sb ++= s"$name\n"
    packages.sortBy(_.name).foreach(_.prettyPrintWith(indent + 4, sb))
    sources.foreach { s =>
      sb ++= (" " * (indent + 4))
      sb ++= Option(AbstractFile.getFile(s)).map(_.name).getOrElse(s) + "\n"
    }
    sb
  }
}

/**
 * Represent a package and its contents in a way that's close to the file system.
 *
 * A package contains any number of nested packages and source files. It is mutable in order to allow adding
 * members at any level, as they are discovered by parsing source files.
 *
 * @param name simple name of the package
 * @note This class is not thread safe
 */
class ParsedLogicalPackage(
    val name: String,
    val parent: Option[ParsedLogicalPackage]
) extends LogicalPackage {
  require(
    (name.trim.isEmpty && parent.isEmpty) || (name.trim.nonEmpty && parent.nonEmpty),
    s"Unexpected package name `$name` and parent `$parent`."
  )

  import scala.collection.mutable

  def this(name: String, parent: ParsedLogicalPackage) =
    this(name, Some(parent))

  private val subpackages =
    mutable.LinkedHashMap.empty[String, ParsedLogicalPackage]
  private val directSources = mutable.ListBuffer.empty[String]

  def fullName: String = {
    if (parent.isEmpty || parent.get.name.isEmpty) name
    else s"${parent.get.fullName}.$name"
  }

  def removeEmptyPackages(): Unit = {
    subpackages.values.foreach(_.removeEmptyPackages())
    if (subpackages.isEmpty && directSources.isEmpty) {
      parent.foreach(_.subpackages.remove(name))
    }
  }

  /**
   * Return the existing member package, or create a new one and add it to this package.
   *
   * @param name simple name of the package to be added or looked up
   */
  def enterPackage(name: String): ParsedLogicalPackage = synchronized {
    subpackages.get(name) match {
      case Some(p) => p
      case None =>
        val p = new ParsedLogicalPackage(name, this)
        subpackages(name) = p
        p
    }
  }

  /** Return this directly nested package, if it exists */
  def getPackage(name: String): Option[ParsedLogicalPackage] =
    subpackages.get(name)

  /** Add a source file to this package */
  def enterSource(fileName: String): this.type = synchronized {
    directSources += fileName
    this
  }

  /** Return all member packages. Only direct members are returned. */
  def packages: Seq[ParsedLogicalPackage] = subpackages.values.toList

  /**
   * Return all sources contained by this package. Only direct members are returned, and there are no duplicates.
   *
   * The return type is a sequence and not a Set in order to have deterministic runs
   */
  def sources: Seq[String] = directSources.toSeq.distinct

  override def toString(): String =
    s"package $name(${packages.size} packages and ${sources.size} files)"
}

/**
 * A helper object to parse sourcepaths and extract logical packages.
 */
object ParsedLogicalPackage {
  val disallowedPackages: Set[String] = Set("scala", "scala.test", "_empty_")
  // should be configurable as a user setting
  val excludedPaths: Set[String] = Set(
    "/experimental/",
    "caching/oss/universe/"
  )
  val validExtensions: Set[String] = Set(".scala", ".java")

  def fromMbtIndex(packages: ju.Map[String, ju.Set[Path]]): LogicalPackage = {
    val root = new ParsedLogicalPackage("", None)

    def isSupported(path: Path): Boolean = {
      val filename = path.getFileName.toString
      (!excludedPaths.exists(path.toString.contains) && validExtensions.exists(
        filename.endsWith
      ))
    }

    def enterNestedPackage(pkg: String) = {
      val parts = pkg.split('/')
      var current = root
      for (part <- parts if !part.isEmpty) {
        current = current.enterPackage(part)
      }
      current
    }

    import scala.jdk.CollectionConverters.*
    for ((pkg, paths) <- packages.asScala) {
      val p = enterNestedPackage(pkg)
      // we don't enter anything in the empty package, which is special and generally not useful (it is not)
      // visible from other packages. Similarly, the scala package is special and we don't want to enter any
      // symbols into it, since they might hide standard library symbols, such as scala.Option.
      if (p.name.nonEmpty && !disallowedPackages.contains(p.fullName)) {
        for (path <- paths.asScala if isSupported(path)) {
          p.enterSource(path.toString)
        }
      }
    }

    root.removeEmptyPackages()
    root
  }
}
