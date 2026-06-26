/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import java.io.{File => JFile}
import java.net.{URI, URL}
import java.nio.file.{FileSystems, Files}

import dotty.tools.dotc.classpath.PackageNameUtils.{packageContains, separatePkgAndClassNames}
import dotty.tools.io.{AbstractFile, PlainFile, ClassPath}
import FileUtils.*
import PlainFile.toPlainFile

import scala.jdk.CollectionConverters.*
import scala.collection.immutable.ArraySeq

/**
 * A trait allowing to look for classpath entries in directories. It provides common logic for
 * classes handling class and source files.
 * It makes use of the fact that in the case of nested directories it's easy to find a file
 * when we have a name of a package.
 * It abstracts over the file representation to work with both JFile and AbstractFile.
 */
trait DirectoryLookup[FileEntryType] extends ClassPath {
  type F

  val dir: F

  protected def emptyFiles: Array[F] // avoids reifying ClassTag[F]
  protected def getSubDir(dirName: String): Option[F]
  protected def listChildren(dir: F, filter: Option[F => Boolean] = None): Array[F]
  protected def getName(f: F): String
  protected def toAbstractFile(f: F): AbstractFile
  protected def isPackage(f: F): Boolean

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def isMatchingFile(f: F): Boolean

  private def getDirectory(forPackage: String): Option[F] =
    if (forPackage == ClassPath.RootPackage)
      Some(dir)
    else
      getSubDir(PackageNameUtils.dirPathTrailingSlash(forPackage))

  override def hasPackage(pkg: String): Boolean = getDirectory(pkg).isDefined

  override def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val nestedDirs: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isPackage))
    }
    ArraySeq.unsafeWrapArray(nestedDirs).map(f => PackageEntry(PackageNameUtils.entryName(inPackage, getName(f))))
  }

  protected def files(inPackage: String): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isMatchingFile))
    }
    files.iterator.map(f => createFileEntry(toAbstractFile(f))).toSeq
  }
}

trait JFileDirectoryLookup[FileEntryType] extends DirectoryLookup[FileEntryType] {
  type F = JFile

  protected def emptyFiles: Array[JFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[JFile] = {
    val packageDir = new JFile(dir, packageDirName)
    if (packageDir.exists && packageDir.isDirectory) Some(packageDir)
    else None
  }
  protected def listChildren(dir: JFile, filter: Option[JFile => Boolean]): Array[JFile] = {
    val listing = filter match {
      case Some(f) => dir.listFiles(file => f(file))
      case None => dir.listFiles()
    }

    if (listing != null) {
      // Sort by file name for stable order of directory .class entries in package scope.
      // This gives stable results ordering of base type sequences for unrelated classes
      // with the same base type depth.
      //
      // Notably, this will stably infer `Product with Serializable`
      // as the type of `case class C(); case class D(); List(C(), D()).head`, rather than the opposite order.
      // On Mac, the HFS performs this sorting transparently, but on Linux the order is unspecified.
      //
      // Note this behaviour can be enabled in javac with `javac -XDsortfiles`, but that's only
      // intended to improve determinism of the compiler for compiler hackers.
      java.util.Arrays.sort(listing,
        new java.util.Comparator[JFile] {
          def compare(o1: JFile, o2: JFile) = o1.getName.compareTo(o2.getName)
        })
      listing
    }
    else Array()
  }
  protected def getName(f: JFile): String = f.getName
  protected def toAbstractFile(f: JFile): AbstractFile = f.toPath.toPlainFile
  protected def isPackage(f: JFile): Boolean = f.isPackage

  assert(dir.asInstanceOf[JFile | Null] != null, "Directory file in DirectoryFileLookup cannot be null")

  override def asURLs: Seq[URL] = Seq(dir.toURI.toURL)
}

object JrtClassPath {
  import java.nio.file.*, java.net.URI, scala.util.Properties
  def apply(release: Option[String]): Option[ClassPath] =
    // Longer term we'd like an official API for this in the JDK
    // Discussion: http://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/thread.html#11738
    release match
      case Some(v) if v.toInt < Runtime.version().feature() =>
        try
          val ctSym = Paths.get(Properties.javaHome).resolve("lib").resolve("ct.sym")
          if (Files.notExists(ctSym)) None
          else Some(new CtSymClassPath(ctSym, v.toInt))
        catch case _: Exception => None
      case _ =>
        try Some(new JrtClassPath(FileSystems.getFileSystem(URI.create("jrt:/"))))
        catch case _: ProviderNotFoundException | _: FileSystemNotFoundException => None
  end apply
}

/**
  * Implementation `ClassPath` based on the JDK 9 encapsulated runtime modules (JEP-220)
  *
  * https://bugs.openjdk.java.net/browse/JDK-8066492 is the most up to date reference
  * for the structure of the jrt:// filesystem.
  *
  * The implementation assumes that no classes exist in the empty package.
  */
final class JrtClassPath(fs: java.nio.file.FileSystem) extends ClassPath {
  import java.nio.file.Path, java.nio.file.*
  type F = Path
  private val dir: Path = fs.getPath("/packages")

  // e.g. "java.lang" -> Seq("/modules/java.base")
  private val packageToModuleBases: Map[String, Seq[Path]] = {
    val ps = Files.newDirectoryStream(dir).iterator().asScala
    def lookup(pack: Path): Seq[Path] =
      Files.list(pack).iterator().asScala.map(l => if (Files.isSymbolicLink(l)) Files.readSymbolicLink(l) else l).toList
    ps.map(p => (p.toString.stripPrefix("/packages/"), lookup(p))).toMap
  }

  /** Empty string represents root package */
  override def hasPackage(pkg: String): Boolean = packageToModuleBases.contains(pkg)

  override def packages(inPackage: String): Seq[PackageEntry] =
    packageToModuleBases.keysIterator.filter(pack => packageContains(inPackage, pack)).map(PackageEntry(_)).toVector

  override def classes(inPackage: String): Seq[BinaryFileEntry] =
    if (inPackage == ClassPath.RootPackage) Nil
    else
      packageToModuleBases.getOrElse(inPackage, Nil).flatMap(x =>
        Files.list(x.resolve(PackageNameUtils.dirPathTrailingSlash(inPackage))).iterator().asScala.filter(_.getFileName.toString.endsWith(".class"))).map(x =>
        ClassFileEntry(x.toPlainFile)).toVector

  override def asURLs: Seq[URL] = Seq(new URI("jrt:/").toURL)

  override def findClassFile(className: String): Option[AbstractFile] =
    if (!className.contains(".")) None
    else {
      val (inPackage, _) = separatePkgAndClassNames(className)
      packageToModuleBases.getOrElse(inPackage, Nil).iterator.flatMap{ x =>
        val file = x.resolve(FileUtils.dirPath(className) + ".class")
        if (Files.exists(file)) {
          file.toPlainFile :: Nil
        } else Nil
      }.take(1).toList.headOption
    }
}

/**
  * Implementation `ClassPath` based on the \$JAVA_HOME/lib/ct.sym backing http://openjdk.java.net/jeps/247
  */
final class CtSymClassPath(ctSym: java.nio.file.Path, release: Int) extends ClassPath {
  import java.nio.file.Path, java.nio.file.*

  private val fileSystem: FileSystem = FileSystems.newFileSystem(ctSym, null: ClassLoader | Null)
  private val root: Path = fileSystem.getRootDirectories.iterator.next
  private val roots = Files.newDirectoryStream(root).iterator.asScala.toList

  // http://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/011737.html
  private def codeFor(major: Int): String = if (major < 10) major.toString else ('A' + (major - 10)).toChar.toString

  private val releaseCode: String = codeFor(release)
  private def fileNameMatchesRelease(fileName: String) = !fileName.contains("-") && fileName.contains(releaseCode) // exclude `9-modules`
  private val rootsForRelease: List[Path] = roots.filter(root => fileNameMatchesRelease(root.getFileName.toString))

  // e.g. "java.lang" -> Seq(/876/java/lang, /87/java/lang, /8/java/lang))
  private val packageIndex: scala.collection.Map[String, scala.collection.Seq[Path]] = {
    val index = collection.mutable.HashMap[String, collection.mutable.ListBuffer[Path]]()
    rootsForRelease.foreach(root => Files.walk(root).iterator().asScala.filter(Files.isDirectory(_)).foreach { p =>
      if (p.getNameCount > root.getNameCount + 1) {
        val packageDotted = p.subpath(1 + root.getNameCount, p.getNameCount).toString.replace('/', '.')
        index.getOrElseUpdate(packageDotted, new collection.mutable.ListBuffer) += p
      }
    })
    index
  }

  /** Empty string represents root package */
  override def hasPackage(pkg: String) = packageIndex.contains(pkg)
  override def packages(inPackage: String): Seq[PackageEntry] = {
    packageIndex.keysIterator.filter(pack => packageContains(inPackage, pack)).map(PackageEntry(_)).toVector
  }
  override def classes(inPackage: String): Seq[BinaryFileEntry] = {
    if (inPackage == ClassPath.RootPackage) Nil
    else {
      val sigFiles = packageIndex.getOrElse(inPackage, Nil).iterator.flatMap(p =>
        Files.list(p).iterator.asScala.filter(_.getFileName.toString.endsWith(".sig")))
      sigFiles.map(f => ClassFileEntry(f.toPlainFile)).toVector
    }
  }

  override def findClassFile(className: String): Option[AbstractFile] = {
    if (!className.contains(".")) None
    else {
      val (inPackage, classSimpleName) = separatePkgAndClassNames(className)
      packageIndex.getOrElse(inPackage, Nil).iterator.flatMap { p =>
        val path = p.resolve(classSimpleName + ".sig")
        if (Files.exists(path)) path.toPlainFile :: Nil else Nil
      }.take(1).toList.headOption
    }
  }
}

case class DirectoryClassPath(dir: JFile) extends JFileDirectoryLookup[BinaryFileEntry] {

  override def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val classFile = new JFile(dir, relativePath + ".class")
    if classFile.exists then {
      Some(classFile.toPath.toPlainFile)
    } else None
  }

  protected def createFileEntry(file: AbstractFile): BinaryFileEntry = BinaryFileEntry(file)

  protected def isMatchingFile(f: JFile): Boolean =
    f.isTasty || f.isBestEffortTasty || (f.isClass && !f.hasSiblingTasty)

  override def classes(inPackage: String): Seq[BinaryFileEntry] = files(inPackage)
}

case class DirectorySourcePath(dir: JFile) extends JFileDirectoryLookup[SourceFileEntry] {
  protected def createFileEntry(file: AbstractFile): SourceFileEntry = SourceFileEntry(file)
  protected def isMatchingFile(f: JFile): Boolean = endsSourceExtension(f.getName)

  override def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)
}
