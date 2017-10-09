package dotty.tools.dotc.classpath

import dotty.tools.io.ClassRepresentation
import dotty.tools.io.{AbstractFile, Path, PlainFile, VirtualDirectory}
import FileUtils._
import java.net.URL

import dotty.tools.io.ClassPath

import dotty.uoption._

case class VirtualDirectoryClassPath(dir: VirtualDirectory) extends ClassPath with DirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  type F = AbstractFile

  // From AbstractFileClassLoader
  private final def lookupPath(base: AbstractFile)(pathParts: Seq[String], directory: Boolean): AbstractFile = {
    var file: AbstractFile = base
    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, directory = true)
      if (file == null)
        return null
    }

    file.lookupName(pathParts.last, directory = directory)
  }

  protected def emptyFiles: Array[AbstractFile] = Array.empty
  protected def getSubDir(packageDirName: String): UOption[AbstractFile] =
    UOption(lookupPath(dir)(packageDirName.split('/'), directory = true))
  protected def listChildren(dir: AbstractFile, filter: UOption[AbstractFile => Boolean] = UNone): Array[F] = filter match {
    case USome(f) => dir.iterator.filter(f).toArray
    case _ => dir.toArray
  }
  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  // mimic the behavior of the old nsc.util.DirectoryClassPath
  def asURLs: Seq[URL] = Seq(new URL(dir.name))
  def asClassPathStrings: Seq[String] = Seq(dir.path)

  override def findClass(className: String): UOption[ClassRepresentation] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): UOption[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className) + ".class"
    UOption(lookupPath(dir)(relativePath split '/', directory = false))
  }

  private[dotty] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: AbstractFile): Boolean = f.isClass
}
