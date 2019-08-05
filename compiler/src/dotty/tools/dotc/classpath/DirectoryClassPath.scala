/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import java.io.{File => JFile}
import java.net.URL
import java.nio.file.{FileSystems, Files}

import dotty.tools.io.{AbstractFile, PlainFile, ClassPath, ClassRepresentation}
import FileUtils._
import scala.collection.JavaConverters._

/**
 * A trait allowing to look for classpath entries in directories. It provides common logic for
 * classes handling class and source files.
 * It makes use of the fact that in the case of nested directories it's easy to find a file
 * when we have a name of a package.
 * It abstracts over the file representation to work with both JFile and AbstractFile.
 */
trait DirectoryLookup[FileEntryType <: ClassRepresentation] extends ClassPath {
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

  private def getDirectory(forPackage: String): Option[F] = {
    if (forPackage == ClassPath.RootPackage) {
      Some(dir)
    } else {
      val packageDirName = FileUtils.dirPath(forPackage)
      getSubDir(packageDirName)
    }
  }

  override private[dotty] def hasPackage(pkg: String): Boolean = getDirectory(pkg).isDefined

  private[dotty] def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val nestedDirs: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isPackage))
    }
    val prefix = PackageNameUtils.packagePrefix(inPackage)
    nestedDirs.map(f => PackageEntryImpl(prefix + getName(f)))
  }

  protected def files(inPackage: String): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isMatchingFile))
    }
    files.iterator.map(f => createFileEntry(toAbstractFile(f))).toSeq
  }

  private[dotty] def list(inPackage: String): ClassPathEntries = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory)
    }
    val packagePrefix = PackageNameUtils.packagePrefix(inPackage)
    val packageBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val fileBuf = collection.mutable.ArrayBuffer.empty[FileEntryType]
    for (file <- files) {
      if (isPackage(file))
        packageBuf += PackageEntryImpl(packagePrefix + getName(file))
      else if (isMatchingFile(file))
        fileBuf += createFileEntry(toAbstractFile(file))
    }
    ClassPathEntries(packageBuf, fileBuf)
  }
}

trait JFileDirectoryLookup[FileEntryType <: ClassRepresentation] extends DirectoryLookup[FileEntryType] {
  type F = JFile

  protected def emptyFiles: Array[JFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[JFile] = {
    val packageDir = new JFile(dir, packageDirName)
    if (packageDir.exists && packageDir.isDirectory) Some(packageDir)
    else None
  }
  protected def listChildren(dir: JFile, filter: Option[JFile => Boolean]): Array[JFile] = {
    val listing = filter match {
      case Some(f) => dir.listFiles(mkFileFilter(f))
      case None => dir.listFiles()
    }

    if (listing != null) {
      // Sort by file name for stable order of directory .class entries in package scope.
      // This gives stable results ordering of base type sequences for unrelated classes
      // with the same base type depth.
      //
      // Notably, this will stably infer`Product with Serializable`
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
    } else Array()
  }
  protected def getName(f: JFile): String = f.getName
  protected def toAbstractFile(f: JFile): AbstractFile = new PlainFile(new dotty.tools.io.File(f.toPath))
  protected def isPackage(f: JFile): Boolean = f.isPackage

  assert(dir != null, "Directory file in DirectoryFileLookup cannot be null")

  def asURLs: Seq[URL] = Seq(dir.toURI.toURL)
  def asClassPathStrings: Seq[String] = Seq(dir.getPath)
}

object JrtClassPath {
  import java.nio.file._, java.net.URI
  def apply(): Option[ClassPath] = {
    try {
      val fs = FileSystems.getFileSystem(URI.create("jrt:/"))
      Some(new JrtClassPath(fs))
    } catch {
      case _: ProviderNotFoundException | _: FileSystemNotFoundException =>
        None
    }
  }
}

/**
  * Implementation `ClassPath` based on the JDK 9 encapsulated runtime modules (JEP-220)
  *
  * https://bugs.openjdk.java.net/browse/JDK-8066492 is the most up to date reference
  * for the structure of the jrt:// filesystem.
  *
  * The implementation assumes that no classes exist in the empty package.
  */
final class JrtClassPath(fs: java.nio.file.FileSystem) extends ClassPath with NoSourcePaths {
  import java.nio.file.Path, java.nio.file._
  type F = Path
  private val dir: Path = fs.getPath("/packages")

  // e.g. "java.lang" -> Seq("/modules/java.base")
  private val packageToModuleBases: Map[String, Seq[Path]] = {
    val ps = Files.newDirectoryStream(dir).iterator().asScala
    def lookup(pack: Path): Seq[Path] = {
      Files.list(pack).iterator().asScala.map(l => if (Files.isSymbolicLink(l)) Files.readSymbolicLink(l) else l).toList
    }
    ps.map(p => (p.toString.stripPrefix("/packages/"), lookup(p))).toMap
  }

  /** Empty string represents root package */
  override private[dotty] def hasPackage(pkg: String): Boolean = packageToModuleBases.contains(pkg)

  override private[dotty] def packages(inPackage: String): Seq[PackageEntry] = {
    def matches(packageDottedName: String) =
      if (packageDottedName.contains("."))
        packageOf(packageDottedName) == inPackage
      else inPackage == ""
    packageToModuleBases.keysIterator.filter(matches).map(PackageEntryImpl(_)).toVector
  }
  private[dotty] def classes(inPackage: String): Seq[ClassFileEntry] = {
    if (inPackage == "") Nil
    else {
      packageToModuleBases.getOrElse(inPackage, Nil).flatMap(x =>
        Files.list(x.resolve(FileUtils.dirPath(inPackage))).iterator().asScala.filter(_.getFileName.toString.endsWith(".class"))).map(x =>
        ClassFileEntryImpl(new PlainFile(new dotty.tools.io.File(x)))).toVector
    }
  }

  override private[dotty] def list(inPackage: String): ClassPathEntries =
    if (inPackage == "") ClassPathEntries(packages(inPackage), Nil)
    else ClassPathEntries(packages(inPackage), classes(inPackage))

  def asURLs: Seq[URL] = Seq(dir.toUri.toURL)
  // We don't yet have a scheme to represent the JDK modules in our `-classpath`.
  // java models them as entries in the new "module path", we'll probably need to follow this.
  def asClassPathStrings: Seq[String] = Nil

  def findClassFile(className: String): Option[AbstractFile] = {
    if (!className.contains(".")) None
    else {
      val inPackage = packageOf(className)
      packageToModuleBases.getOrElse(inPackage, Nil).iterator.flatMap{x =>
        val file = x.resolve(FileUtils.dirPath(className) + ".class")
        if (Files.exists(file)) new PlainFile(new dotty.tools.io.File(file)) :: Nil else Nil
      }.take(1).toList.headOption
    }
  }
  private def packageOf(dottedClassName: String): String =
    dottedClassName.substring(0, dottedClassName.lastIndexOf("."))
}

case class DirectoryClassPath(dir: JFile) extends JFileDirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  override def findClass(className: String): Option[ClassRepresentation] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val classFile = new JFile(dir, relativePath + ".class")
    if (classFile.exists) {
      val wrappedClassFile = new dotty.tools.io.File(classFile.toPath)
      val abstractClassFile = new PlainFile(wrappedClassFile)
      Some(abstractClassFile)
    } else None
  }

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: JFile): Boolean = f.isClass

  private[dotty] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)
}

case class DirectorySourcePath(dir: JFile) extends JFileDirectoryLookup[SourceFileEntryImpl] with NoClassPaths {
  def asSourcePathString: String = asClassPathString

  protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
  protected def isMatchingFile(f: JFile): Boolean = endsScalaOrJava(f.getName)

  override def findClass(className: String): Option[ClassRepresentation] = findSourceFile(className) map SourceFileEntryImpl

  private def findSourceFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val sourceFile = Stream("scala", "java")
      .map(ext => new JFile(dir, relativePath + "." + ext))
      .collectFirst { case file if file.exists() => file }

    sourceFile.map { file =>
      val wrappedSourceFile = new dotty.tools.io.File(file.toPath)
      val abstractSourceFile = new PlainFile(wrappedSourceFile)
      abstractSourceFile
    }
  }

  private[dotty] def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)
}
