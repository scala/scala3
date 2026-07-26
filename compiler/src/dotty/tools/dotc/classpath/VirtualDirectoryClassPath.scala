package dotty.tools.dotc.classpath

import dotty.tools.io.{AbstractFile, ClassPath, FileExtension, VirtualDirectory}
import FileUtils.*

import java.net.{URI, URL}

case class VirtualDirectoryClassPath(dir: AbstractFile) extends ClassPath with DirectoryLookup[BinaryFileEntry] {
  type F = AbstractFile

  protected def emptyFiles: Array[AbstractFile] = Array.empty

  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    dir.lookupPath(packageDirName, java.io.File.separatorChar, directory = true)

  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean] = None): Array[F] = filter match {
    case Some(f) => dir.iterator.filter(f).toArray
    case _ => dir.iterator.toArray
  }
  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  override def asURLs: Seq[URL] = dir.toURL.toSeq

  override def findClassFile(className: String): Option[AbstractFile] = {
    dir.lookupPath(className, '.', lastSuffix = FileExtension.Class.withDot, directory = false)
  }

  override def classes(inPackage: String): Seq[BinaryFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): BinaryFileEntry = BinaryFileEntry(file)

  protected def isMatchingFile(f: AbstractFile): Boolean = {
    f.exists && (f.ext.isTasty || f.ext.isBetasty || (f.ext.isClass && !f.hasSiblingTasty))
  }
}
