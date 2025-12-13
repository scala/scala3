package dotc.io

/**
 * Cross-platform abstract file representation.
 *
 * This is a simplified version of dotty.tools.io.AbstractFile that works
 * in both JVM and JavaScript environments.
 */
abstract class AbstractFile {

  /** The name of this file (without path) */
  def name: String

  /** The path of this file */
  def path: String

  /** The absolute path of this file */
  def absolutePath: String = path

  /** The parent directory, if any */
  def container: AbstractFile

  /** Is this a directory? */
  def isDirectory: Boolean

  /** Is this a virtual file (in-memory)? */
  def isVirtual: Boolean = true

  /** Does this file exist? */
  def exists: Boolean = true

  /** The file extension, or empty string */
  def extension: String = {
    val idx = name.lastIndexOf('.')
    if (idx >= 0) name.substring(idx + 1) else ""
  }

  /** Check if this is a TASTy file */
  def hasTastyExtension: Boolean = extension == "tasty"

  /** Check if this is a best-effort TASTy file */
  def hasBetastyExtension: Boolean = extension == "betasty"

  /** Check if this is a class file */
  def hasClassExtension: Boolean = extension == "class"

  /** Check if this is a Scala source file */
  def hasScalaExtension: Boolean = extension == "scala"

  /** The file contents as a byte array */
  def toByteArray: Array[Byte]

  /** The file contents as a character array */
  def toCharArray: Array[Char] = {
    val bytes = toByteArray
    new String(bytes, "UTF-8").toCharArray
  }

  /** The file contents as a string */
  def content: String = new String(toByteArray, "UTF-8")

  /** Look up a child by name */
  def lookupName(name: String, directory: Boolean): AbstractFile | Null = null

  /** Look up or create a child directory */
  def subdirectoryNamed(name: String): AbstractFile =
    throw new UnsupportedOperationException(s"Cannot create subdirectory in $path")

  /** Look up or create a child file */
  def fileNamed(name: String): AbstractFile =
    throw new UnsupportedOperationException(s"Cannot create file in $path")

  /** Iterator over children (for directories) */
  def iterator: Iterator[AbstractFile] = Iterator.empty

  /** Resolve a sibling file */
  def resolveSibling(name: String): AbstractFile | Null = {
    if (container != null) container.lookupName(name, directory = false)
    else null
  }

  /** String representation */
  override def toString: String = path

  /** Equality based on path */
  override def equals(obj: Any): Boolean = obj match {
    case other: AbstractFile => path == other.path
    case _ => false
  }

  override def hashCode: Int = path.hashCode
}

/**
 * A virtual file that exists only in memory.
 */
class VirtualFile(
  val name: String,
  val path: String,
  private var _content: Array[Byte],
  val container: AbstractFile
) extends AbstractFile {

  def this(name: String, content: Array[Byte]) =
    this(name, name, content, null)

  def this(name: String, content: String) =
    this(name, content.getBytes("UTF-8"))

  override def isDirectory: Boolean = false

  override def toByteArray: Array[Byte] = _content

  /** Update the file content */
  def setContent(content: Array[Byte]): Unit = _content = content
  def setContent(content: String): Unit = setContent(content.getBytes("UTF-8"))
}

/**
 * A virtual directory that exists only in memory.
 */
class VirtualDirectory(
  val name: String,
  val container: AbstractFile
) extends AbstractFile {

  def this(name: String) = this(name, null)

  private val children = scala.collection.mutable.Map[String, AbstractFile]()

  override def path: String = {
    if (container == null) name
    else if (container.path.isEmpty) name
    else s"${container.path}/$name"
  }

  override def isDirectory: Boolean = true

  override def toByteArray: Array[Byte] =
    throw new UnsupportedOperationException("Cannot read directory as bytes")

  override def lookupName(name: String, directory: Boolean): AbstractFile | Null = {
    children.get(name) match {
      case Some(f) if f.isDirectory == directory => f
      case _ => null
    }
  }

  override def subdirectoryNamed(name: String): AbstractFile = {
    children.getOrElseUpdate(name, new VirtualDirectory(name, this))
  }

  override def fileNamed(name: String): AbstractFile = {
    children.getOrElseUpdate(name, new VirtualFile(name, s"$path/$name", Array.empty, this))
  }

  /** Add a file to this directory */
  def add(file: AbstractFile): Unit = {
    children(file.name) = file
  }

  /** Add a file with content */
  def addFile(name: String, content: Array[Byte]): VirtualFile = {
    val file = new VirtualFile(name, s"$path/$name", content, this)
    children(name) = file
    file
  }

  def addFile(name: String, content: String): VirtualFile =
    addFile(name, content.getBytes("UTF-8"))

  override def iterator: Iterator[AbstractFile] = children.valuesIterator

  /** Clear all children */
  def clear(): Unit = children.clear()
}

/**
 * Companion object with utilities.
 */
object AbstractFile {

  /** Create a virtual file from string content */
  def apply(name: String, content: String): VirtualFile =
    new VirtualFile(name, content)

  /** Create a virtual file from byte content */
  def apply(name: String, content: Array[Byte]): VirtualFile =
    new VirtualFile(name, name, content, null)

  /** Create a virtual directory */
  def directory(name: String): VirtualDirectory =
    new VirtualDirectory(name)
}

