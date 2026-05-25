package dotty.tools.dotc.interactive

import scala.collection.mutable
import dotty.tools.io.AbstractFile
import scala.jdk.CollectionConverters.*

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

  def this(name: String, parent: ParsedLogicalPackage) =
    this(name, Some(parent))

  private val subpackages =
    mutable.LinkedHashMap.empty[String, ParsedLogicalPackage]
  private val directSources = mutable.ListBuffer.empty[String]

  def fullName: String =
    if (parent.isEmpty || parent.get.name.isEmpty) name
    else s"${parent.get.fullName}.$name"

  def removeEmptyPackages(): Unit =
    subpackages.values.foreach(_.removeEmptyPackages())
    if subpackages.isEmpty && directSources.isEmpty then
      parent.foreach(_.subpackages.remove(name))


  /**
   * Return the existing member package, or create a new one and add it to this package.
   */
  def enterPackage(name: String): ParsedLogicalPackage = synchronized{
    subpackages.get(name) match {
      case Some(p) => p
      case None =>
        val p = new ParsedLogicalPackage(name, this)
        subpackages(name) = p
        p
    }
  }

  def getPackage(name: String): Option[ParsedLogicalPackage] =
    subpackages.get(name)

  def enterSource(fileName: String): this.type = synchronized:
    directSources += fileName
    this

  /** Return all member packages. Only direct members are returned. */
  def packages: Seq[ParsedLogicalPackage] = subpackages.values.toList

  /**
   * Return all sources contained by this package. Only direct members are returned, and there are no duplicates.
   *
   * The return type is a sequence and not a Set in order to have deterministic runs
   */
  def sources: Seq[AbstractFile] = directSources.toSeq.distinct.flatMap(name => Option(AbstractFile.getFile(name)))

  override def toString(): String =
    s"package $name(${packages.size} packages and ${sources.size} files)"
}

object ParsedLogicalPackage{

  def fromMbtIndex(
      packages: java.util.Map[String, java.util.Set[java.nio.file.Path]]
  ): ParsedLogicalPackage =
    val root = new ParsedLogicalPackage("", None)
    val disallowedPackages: Set[String] = Set("scala", "scala.test", "_empty_")
    val validExtensions: Set[String] = Set(".scala", ".java")

    def isSupported(path: java.nio.file.Path): Boolean =
      val filename = path.getFileName.toString
      validExtensions.exists(filename.endsWith)

    def enterNestedPackage(pkg: String) =
      val parts = pkg.split('/')
      var current = root
      for part <- parts if !part.isEmpty do
        current = current.enterPackage(part)
      current

    for ((pkg, paths) <- packages.asScala) do
      val p = enterNestedPackage(pkg)
      // we don't enter anything in the empty package, which is special and generally not useful (it is not)
      // visible from other packages. Similarly, the scala package is special and we don't want to enter any
      // symbols into it, since they might hide standard library symbols, such as scala.Option.
      if p.name.nonEmpty && !disallowedPackages.contains(p.fullName) then
        for path <- paths.asScala if isSupported(path) do
          p.enterSource(path.toString)

    root.removeEmptyPackages()
    root
}