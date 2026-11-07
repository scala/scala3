package dotty.tools.dotc.classpath

import dotty.tools.nio.*

import scala.collection.mutable

/**
 * Implementation `ClassPath` based on the JDK 9 encapsulated runtime modules (JEP-220)
 *
 * https://bugs.openjdk.java.net/browse/JDK-8066492 is the most up-to-date reference
 * for the structure of the jrt:// filesystem.
 *
 * The implementation assumes that no classes exist in the empty package.
 */
private final class JrtClassPath(packagesDir: FileContainer) extends ClassPath {
  // Right now the compiler always asks for those at the root package anyway (inPackage == ""),
  // and we have no way to query the file system for "entries without a dot in their name",
  // so might as well cache them
  private val allPackages = packagesDir.entries.map(f => f.name).toSeq

  // e.g. "java.lang" -> Seq("/modules/java.base")
  // On a modern JDK there are 100s of packages,
  // most of which are never going to be needed because they're internal implementation details,
  // so we lazy-load modules from the ones we need
  private val cachedPackageToModuleBases = mutable.Map.empty[String, Seq[FileContainer]]
  private def packageToModuleBases(pkg: String): Seq[FileContainer] =
    cachedPackageToModuleBases.get(pkg) match
      case Some(ps) => ps
      case None =>
        val moduleFiles = packagesDir.getContainer(pkg) match
          case Some(c) => c.entries.collect { case c: FileContainer => c }.toSeq
          case None => Seq.empty
        cachedPackageToModuleBases(pkg) = moduleFiles
        moduleFiles

  override def hasPackage(pkg: String): Boolean =
    packageToModuleBases(pkg).nonEmpty

  override def packages(inPackage: String): Iterable[String] =
    if inPackage == "" then
      allPackages.filter(p => !p.contains('.'))
    else
      val start = inPackage + "."
      allPackages.filter(p => p.startsWith(start) && p.lastIndexOf('.') == inPackage.length)

  private val cachedClasses = mutable.Map.empty[String, Map[String, BinaryFileEntry]]
  private def classesByName(inPackage: String): Map[String, BinaryFileEntry] = cachedClasses.get(inPackage) match {
    case Some(cs) => cs
    case None =>
      val cs = packageToModuleBases(inPackage)
        .flatMap(pkg => pkg.getContainer(inPackage, separator = '.').get.entries)
        .collect { case f: File if f.extension.isClass => (f.name, BinaryFileEntry(f)) }
        .toMap
      cachedClasses(inPackage) = cs
      cs
  }

  override def classes(inPackage: String): Iterable[BinaryFileEntry] =
    classesByName(inPackage).values

  override def findClassFile(className: String): Option[File] =
    val (pkg, cls) = PackageNameUtils.separatePkgAndClassNames(className)
    // Because the compiler asks about `classes` first and then requests class files,
    // this will in practice be cached
    classesByName(pkg).get(cls + ".class").map(_.file)

  override def searchLocations: Iterable[String] =
    Iterable.empty // not necessary to include JRT in there
}