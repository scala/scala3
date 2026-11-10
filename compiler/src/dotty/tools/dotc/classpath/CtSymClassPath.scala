package dotty.tools.dotc.classpath

import scala.collection.mutable

import dotty.tools.nio.*

/**
  * Implementation `ClassPath` based on the \$JAVA_HOME/lib/ct.sym backing http://openjdk.java.net/jeps/247
  */
private final class CtSymClassPath(root: FileContainer, release: Int) extends ClassPath {
  private val roots = root.entries.collect { case c: FileContainer => c }.toSeq

  // http://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/011737.html
  private def codeFor(major: Int): String = if (major < 10) major.toString else ('A' + (major - 10)).toChar.toString

  private val releaseCode: String = codeFor(release)
  private def fileNameMatchesRelease(fileName: String) = !fileName.contains("-") && fileName.contains(releaseCode) // exclude `9-modules`
  private val rootsForRelease = roots.filter(root => fileNameMatchesRelease(root.name))

  // e.g. "java.lang" -> Seq(/876/java/lang, /87/java/lang, /8/java/lang))
  private val packageIndex: collection.Map[String, collection.Seq[FileContainer]] = {
    val index = mutable.Map.empty[String, mutable.ListBuffer[FileContainer]]
    val subRoots = rootsForRelease.flatMap(_.entries.collect { case c: FileContainer => c })
    subRoots.foreach(subRoot =>
      subRoot.recursiveEntries.foreach {
        case c: FileContainer =>
          val packageDotted = c.pathRelativeTo(subRoot).replace('/', '.')
          index.getOrElseUpdate(packageDotted, new mutable.ListBuffer) += c
        case _ =>
          ()
      }
    )
    index
  }

  /** Empty string represents root package */
  override def hasPackage(pkg: String): Boolean =
    packageIndex.contains(pkg)

  override def packages(inPackage: String): Iterable[String] =
    packageIndex.keys.filter(pack => PackageNameUtils.packageContains(inPackage, pack))

  override def classes(inPackage: String): Iterable[BinaryFileEntry] =
    packageIndex.getOrElse(inPackage, Nil).flatMap(p => p.entries.collect { case f: File if f.extension.isSig => BinaryFileEntry(f) })

  override def findClassFile(className: String): Option[File] =
    val (inPackage, classSimpleName) = PackageNameUtils.separatePkgAndClassNames(className)
    packageIndex.getOrElse(inPackage, Nil).iterator.flatMap(_.getFile(classSimpleName + ".sig")).nextOption()

  override def searchLocations: Iterable[String] =
    Iterable.empty // not necessary to include CT in there
}
