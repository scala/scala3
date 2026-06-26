package dotty.tools.dotc.classpath

import dotty.tools.io.ClassPath
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import FileUtils.*
import java.net.{URI, URL}

case class VirtualDirectoryClassPath(dir: VirtualDirectory) extends ClassPath with DirectoryLookup[BinaryFileEntry] {
  type F = AbstractFile

  // From AbstractFileClassLoader
  private final def lookupPath(base: AbstractFile)(pathParts: Seq[String], directory: Boolean): AbstractFile | Null = {
    var file: AbstractFile = base
    val dirParts = pathParts.init.iterator
    while (dirParts.hasNext) {
      val dirPart = dirParts.next()
      val subFile = file.lookupName(dirPart, directory = true)
      if (subFile == null)
        return null
      file = subFile
    }
    file.lookupName(pathParts.last, directory = directory)
  }

  protected def emptyFiles: Array[AbstractFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    Option(lookupPath(dir)(packageDirName.split(java.io.File.separator).toIndexedSeq, directory = true))
  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean] = None): Array[F] = filter match {
    case Some(f) => dir.iterator.filter(f).toArray
    case _ => dir.toArray
  }
  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  // mimic the behavior of the old nsc.util.DirectoryClassPath
  override def asURLs: Seq[URL] = Seq(new URI(dir.name).toURL)

  override def findClassFile(className: String): Option[AbstractFile] = {
    val pathSeq = FileUtils.dirPath(className).split(java.io.File.separator)
    val parentDir = lookupPath(dir)(pathSeq.init.toSeq, directory = true)
    if parentDir == null then None
    else
      Option(lookupPath(parentDir)(pathSeq.last + ".class" :: Nil, directory = false))
  }

  override def classes(inPackage: String): Seq[BinaryFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): BinaryFileEntry = BinaryFileEntry(file)

  protected def isMatchingFile(f: AbstractFile): Boolean = {
    f.exists && (f.ext.isTasty || (f.ext.isClass && !f.hasSiblingTasty))
  }
}
