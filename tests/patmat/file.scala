abstract class AbstractFile
class PlainFile(path: String) extends AbstractFile
class VirtualFile(name: String) extends AbstractFile
abstract class ZipArchive(path: String) extends AbstractFile {
  sealed abstract class Entry(name: String) extends VirtualFile(name)
  class DirEntry(path: String) extends Entry(path)
}

object Test {
  def foo(file: AbstractFile) =  file match {
    case ze: ZipArchive#Entry =>
    case pf: PlainFile =>
    case _ =>
  }
}