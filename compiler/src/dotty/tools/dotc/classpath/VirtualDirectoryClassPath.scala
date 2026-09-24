package dotty.tools.dotc.classpath

import dotty.tools.io.{AbstractFile, FileExtension}
import FileUtils.*

import java.net.{URI, URL}

case class VirtualDirectoryClassPath(dir: AbstractFile) extends ClassPath with DirectoryLookup[BinaryFileEntry] {
  type F = AbstractFile

  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    dir.lookupPath(packageDirName, java.io.File.separatorChar, directory = true)

  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean] = None): Iterable[F] = filter match {
    case Some(f) => dir.iterator.filter(f).toSeq
    case _ => dir.iterator.toSeq
  }
  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  override def asURLs: Seq[URL] = dir.toURL.toSeq

  override def findClassFile(className: String): Option[AbstractFile] = {
    dir.lookupPath(className, '.', lastSuffix = FileExtension.Class.withDot, directory = false)
  }

  override def classes(inPackage: String): Iterable[BinaryFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): BinaryFileEntry = BinaryFileEntry(file)

  protected def isMatchingFile(f: AbstractFile): Boolean = {
    f.exists && (f.ext.isTasty || f.ext.isBetasty || (f.ext.isClass && !f.hasSiblingTasty))
  }
}
