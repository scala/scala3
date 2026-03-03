abstract class FileSystem extends caps.SharedCapability:
  def access(path: String): FileEntry{val origin: FileSystem.this.type}^{this}
abstract class FileEntry(val origin: FileSystem):
  def path: String
  def name: String
  def exists: Boolean
  def isDirectory: Boolean
  /** Recursively list all descendants (files and subdirectories). */
  def walk(): List[FileEntry^{origin}]



def requestFileSystem[T](root: String)(op: FileSystem => T): T = ???

def test =
  requestFileSystem("/") { fs =>
    val l = fs.access("/").walk().map(f => f.path) // error
    // If we assign `fs.access("/")` to a local variable, it compiles ok
    // If we remove the map, it also compiles ok
  }