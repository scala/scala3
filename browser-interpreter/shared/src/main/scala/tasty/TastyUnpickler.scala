package tasty

import TastyBuffer._
import TastyFormat._

/**
 * Cross-platform TASTy unpickler.
 * Reads TASTy header, name table, and provides section access.
 */
class TastyUnpickler(bytes: Array[Byte]) {

  private val reader = new TastyReader(bytes)

  /** TASTy header information */
  case class Header(
    majorVersion: Int,
    minorVersion: Int,
    experimentalVersion: Int,
    toolingVersion: String,
    uuid: (Long, Long)
  )

  /** A section in the TASTy file */
  case class Section(name: String, start: Addr, end: Addr) {
    def reader: TastyReader = new TastyReader(bytes, start.index, end.index, start.index)
  }

  /** Parsed header */
  var header: Option[Header] = None

  /** Name table */
  private var names: Array[TastyName] = Array.empty

  /** Sections in the TASTy file */
  private var sections: List[Section] = Nil

  /** Read and validate the TASTy file */
  def read(): Boolean = {
    try {
      readHeader()
      readNames()
      readSections()
      true
    } catch {
      case e: Exception =>
        println(s"TASTy read error: ${e.getMessage}")
        false
    }
  }

  /** Read the TASTy header */
  private def readHeader(): Unit = {
    // Check magic number
    for (i <- 0 until TastyFormat.header.length) {
      val b = reader.readByte()
      if (b != TastyFormat.header(i)) {
        throw new RuntimeException(s"Not a TASTy file: bad magic number at byte $i")
      }
    }

    val major = reader.readNat()
    if (major <= 27) {
      throw new RuntimeException(s"TASTy version $major is too old (minimum 28)")
    }

    val minor = reader.readNat()
    val experimental = reader.readNat()

    // Read tooling version string
    val toolingLength = reader.readNat()
    val toolingBytes = reader.readBytes(toolingLength)
    val tooling = new String(toolingBytes.map(_.toChar))

    // Check version compatibility
    if (!TastyFormat.isVersionCompatible(
      major, minor, experimental,
      MajorVersion, MinorVersion, ExperimentalVersion
    )) {
      throw new RuntimeException(
        s"TASTy version $major.$minor.$experimental is incompatible with reader version $MajorVersion.$MinorVersion.$ExperimentalVersion"
      )
    }

    // Read UUID
    val uuidHigh = reader.readUncompressedLong()
    val uuidLow = reader.readUncompressedLong()

    header = Some(Header(major, minor, experimental, tooling, (uuidHigh, uuidLow)))
  }

  /** Read the name table */
  private def readNames(): Unit = {
    val nameTableLength = reader.readNat()
    val nameTableEnd = reader.currentAddr + nameTableLength

    val nameBuffer = scala.collection.mutable.ArrayBuffer[TastyName]()
    nameBuffer += TastyName.Empty // Index 0 is empty

    while (reader.currentAddr.index < nameTableEnd.index) {
      val name = readName()
      nameBuffer += name
    }

    names = nameBuffer.toArray
  }

  /** Read a single name from the name table */
  private def readName(): TastyName = {
    import NameTags._

    val tag = reader.readByte()
    tag match {
      case UTF8 =>
        TastyName.Simple(reader.readUtf8())

      case QUALIFIED =>
        val length = reader.readNat()
        val end = reader.currentAddr + length
        val prefix = reader.readNat()
        val selector = reader.readNat()
        TastyName.Qualified(prefix, selector)

      case EXPANDED =>
        val length = reader.readNat()
        val end = reader.currentAddr + length
        val prefix = reader.readNat()
        val selector = reader.readNat()
        TastyName.Expanded(prefix, selector)

      case EXPANDPREFIX =>
        val length = reader.readNat()
        val end = reader.currentAddr + length
        val prefix = reader.readNat()
        val selector = reader.readNat()
        TastyName.ExpandPrefix(prefix, selector)

      case UNIQUE =>
        val length = reader.readNat()
        val end = reader.currentAddr + length
        val separator = reader.readNat()
        val num = reader.readNat()
        val underlying = if (reader.currentAddr.index < end.index) Some(reader.readNat()) else None
        TastyName.Unique(separator, num, underlying)

      case DEFAULTGETTER =>
        val length = reader.readNat()
        val underlying = reader.readNat()
        val index = reader.readNat()
        TastyName.DefaultGetter(underlying, index)

      case SUPERACCESSOR =>
        val length = reader.readNat()
        val underlying = reader.readNat()
        TastyName.SuperAccessor(underlying)

      case INLINEACCESSOR =>
        val length = reader.readNat()
        val underlying = reader.readNat()
        TastyName.InlineAccessor(underlying)

      case OBJECTCLASS =>
        val length = reader.readNat()
        val underlying = reader.readNat()
        TastyName.ObjectClass(underlying)

      case BODYRETAINER =>
        val length = reader.readNat()
        val underlying = reader.readNat()
        TastyName.BodyRetainer(underlying)

      case SIGNED | TARGETSIGNED =>
        val length = reader.readNat()
        val end = reader.currentAddr + length
        val original = reader.readNat()
        val target = if (tag == TARGETSIGNED) Some(reader.readNat()) else None
        val result = reader.readNat()
        val params = scala.collection.mutable.ListBuffer[Int]()
        while (reader.currentAddr.index < end.index) {
          params += reader.readInt()
        }
        TastyName.Signed(original, target, result, params.toList)

      case _ =>
        throw new RuntimeException(s"Unknown name tag: $tag")
    }
  }

  /** Read sections */
  private def readSections(): Unit = {
    val sectionBuffer = scala.collection.mutable.ListBuffer[Section]()

    while (!reader.isAtEnd) {
      val nameRef = reader.readNat()
      val length = reader.readNat()
      val start = reader.currentAddr
      val end = start + length

      val sectionName = getName(nameRef) match {
        case TastyName.Simple(s) => s
        case _ => s"Section$nameRef"
      }

      sectionBuffer += Section(sectionName, start, end)
      reader.goto(end)
    }

    sections = sectionBuffer.toList
  }

  /** Get a name by index */
  def getName(ref: Int): TastyName = {
    if (ref < 0 || ref >= names.length) TastyName.Empty
    else names(ref)
  }

  /** Get a name by NameRef */
  def getNameByRef(ref: NameRef): TastyName = getName(ref.index)

  /** Resolve a name to its full string representation */
  def nameToString(ref: Int): String = {
    getName(ref) match {
      case TastyName.Empty => ""
      case TastyName.Simple(s) => s
      case TastyName.Qualified(prefix, selector) =>
        s"${nameToString(prefix)}.${nameToString(selector)}"
      case TastyName.Expanded(prefix, selector) =>
        s"${nameToString(prefix)}$$$${nameToString(selector)}"
      case TastyName.ExpandPrefix(prefix, selector) =>
        s"${nameToString(prefix)}$$${nameToString(selector)}"
      case TastyName.Unique(sep, num, underlying) =>
        val base = underlying.map(nameToString).getOrElse("")
        s"$base${nameToString(sep)}$num"
      case TastyName.DefaultGetter(underlying, index) =>
        s"${nameToString(underlying)}$$default$$$index"
      case TastyName.SuperAccessor(underlying) =>
        s"super$$${nameToString(underlying)}"
      case TastyName.InlineAccessor(underlying) =>
        s"inline$$${nameToString(underlying)}"
      case TastyName.ObjectClass(underlying) =>
        s"${nameToString(underlying)}$$"
      case TastyName.BodyRetainer(underlying) =>
        s"${nameToString(underlying)}$$retainedBody"
      case TastyName.Signed(original, _, _, _) =>
        nameToString(original)
    }
  }

  /** Get a section by name */
  def getSection(name: String): Option[Section] =
    sections.find(_.name == name)

  /** Get the ASTs section */
  def getASTsSection: Option[Section] =
    getSection(ASTsSection)

  /** Get all section names */
  def getSectionNames: List[String] =
    sections.map(_.name)
}

/** TASTy name representation */
sealed trait TastyName

object TastyName {
  case object Empty extends TastyName
  case class Simple(name: String) extends TastyName
  case class Qualified(prefix: Int, selector: Int) extends TastyName
  case class Expanded(prefix: Int, selector: Int) extends TastyName
  case class ExpandPrefix(prefix: Int, selector: Int) extends TastyName
  case class Unique(separator: Int, num: Int, underlying: Option[Int]) extends TastyName
  case class DefaultGetter(underlying: Int, index: Int) extends TastyName
  case class SuperAccessor(underlying: Int) extends TastyName
  case class InlineAccessor(underlying: Int) extends TastyName
  case class ObjectClass(underlying: Int) extends TastyName
  case class BodyRetainer(underlying: Int) extends TastyName
  case class Signed(original: Int, target: Option[Int], result: Int, params: List[Int]) extends TastyName
}

