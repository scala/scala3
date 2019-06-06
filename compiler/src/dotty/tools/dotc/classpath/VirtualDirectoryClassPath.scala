package dotty.tools.dotc.classpath

import dotty.tools.io.ClassRepresentation
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import FileUtils._
import java.net.URL

import dotty.tools.io.ClassPath

case class VirtualDirectoryClassPath(dir: VirtualDirectory) extends ClassPath with DirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  type F = AbstractFile

  // From AbstractFileClassLoader
  private final def lookupPath(base: AbstractFile)(pathParts: Seq[String], directory: Boolean): AbstractFile = {
    var file: AbstractFile = base
    val dirParts = pathParts.init.toIterator
    while (dirParts.hasNext) {
      val dirPart = dirParts.next
      file = file.lookupName(dirPart, directory = true)
      if (file == null)
        return null
    }
    file.lookupName(pathParts.last, directory = directory)
  }

  protected def emptyFiles: Array[AbstractFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    Option(lookupPath(dir)(packageDirName.split(java.io.File.separator), directory = true))
  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean] = None): Array[F] = filter match {
    case Some(f) => dir.iterator.filter(f).toArray
    case _ => dir.toArray
  }
  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  // mimic the behavior of the old nsc.util.DirectoryClassPath
  def asURLs: Seq[URL] = Seq(new URL(dir.name))
  def asClassPathStrings: Seq[String] = Seq(dir.path)

  override def findClass(className: String): Option[ClassRepresentation] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className) + ".class"
    Option(lookupPath(dir)(relativePath.split(java.io.File.separator), directory = false))
  }

  private[dotty] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: AbstractFile): Boolean = f.isClass
}
