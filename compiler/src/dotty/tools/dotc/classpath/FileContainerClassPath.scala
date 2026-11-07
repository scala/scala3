package dotty.tools.dotc.classpath

import dotty.tools.nio.*

private final class FileContainerClassPath(root: FileContainer) extends ClassPath:
  override def hasPackage(pkg: String): Boolean =
    getPackageContainer(pkg).nonEmpty

  override def packages(inPackage: String): Iterable[String] =
    getPackageContainer(inPackage) match
      case Some(c) => c.entries.collect { case c: FileContainer => inPackage + "." + c.path }
      case None => Iterable.empty

  override def searchLocations: Iterable[String] =
    Iterable(root.path)

  override def classes(inPackage: String): Iterable[BinaryFileEntry] =
    getPackageContainer(inPackage) match
      case Some(c) => sortEntries(c.entries.collect { case f: File if isClass(f) => BinaryFileEntry(f) })
      case None => Iterable.empty

  override def sources(inPackage: String): Iterable[SourceFileEntry] =
    getPackageContainer(inPackage) match
      case Some(c) => sortEntries(c.entries.collect { case f: File if isSource(f) => SourceFileEntry(f) })
      case None => Iterable.empty

  override def findClassFile(className: String): Option[File] =
    root.getFile(className, "class", separator = '.')

  private def getPackageContainer(pkg: String): Option[FileContainer] =
    if pkg == ClassPath.RootPackage then Some(root)
    else root.getContainer(pkg, separator = '.')

  private def isClass(f: File): Boolean =
    f.extension.isTasty || f.extension.isBetasty || (f.extension.isClass && f.parent.getFile(ClassPath.classNameToTasty(f.name)).isEmpty)

  private def isSource(f: File): Boolean =
    f.extension.isScala || f.extension.isJava

  private inline def sortEntries[T <: ClassRepresentation](entries: Iterable[T]): Iterable[T] =
    // Sort by file name for stable order of directory .class entries in package scope.
    // This gives stable results ordering of base type sequences for unrelated classes
    // with the same base type depth.
    //
    // Notably, this will stably infer `Product with Serializable`
    // as the type of `case class C(); case class D(); List(C(), D()).head`, rather than the opposite order.
    // On Mac, the HFS performs this sorting transparently, but on Linux the order is unspecified.
    //
    // Note this behavior can be enabled in javac with `javac -XDsortfiles`, but that's only
    // intended to improve determinism of the compiler for compiler hackers.
    entries.toList.sortBy(_.name)
