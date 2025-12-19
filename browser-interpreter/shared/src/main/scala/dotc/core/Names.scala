package dotc.core

import scala.collection.mutable

/**
 * Cross-platform name representation for the browser compiler.
 *
 * Names in Scala 3 are interned strings with additional type information.
 * This simplified version provides the essential functionality for parsing.
 */
object Names {

  /** The name table for interning simple names */
  private val nameTable = mutable.HashMap[String, SimpleName]()

  /** A common type of Name and Symbol */
  type Designator = Name

  /** Things that can be turned into names */
  type PreName = Name | String

  /** Base class for all names */
  sealed abstract class Name {

    /** Is this a type name? */
    def isTypeName: Boolean

    /** Is this a term name? */
    def isTermName: Boolean = !isTypeName

    /** Convert to type name */
    def toTypeName: TypeName

    /** Convert to term name */
    def toTermName: TermName

    /** The underlying string */
    def toString: String

    /** Is this name empty? */
    def isEmpty: Boolean = toString.isEmpty

    /** Does this name start with the given prefix? */
    def startsWith(prefix: String): Boolean = toString.startsWith(prefix)

    /** Does this name end with the given suffix? */
    def endsWith(suffix: String): Boolean = toString.endsWith(suffix)

    /** The length of this name */
    def length: Int = toString.length

    /** Get character at index */
    def apply(index: Int): Char = toString.charAt(index)

    /** The first part of this name (for qualified names) */
    def firstPart: SimpleName

    /** The last part of this name (for qualified names) */
    def lastPart: SimpleName

    /** Concatenate with another name or string */
    def ++ (other: String): Name
    def ++ (other: Name): Name = this ++ other.toString

    /** Check equality */
    override def equals(that: Any): Boolean = that match {
      case name: Name => this.toString == name.toString && this.isTypeName == name.isTypeName
      case _ => false
    }

    override def hashCode: Int = toString.hashCode * (if (isTypeName) 31 else 1)

    /** Encode operator symbols */
    def encode: Name = this

    /** Decode operator symbols */
    def decode: Name = this

    /** Mangle name for JVM */
    def mangled: Name = this
    def mangledString: String = toString

    /** The simple name underlying this name */
    def asSimpleName: SimpleName
    def toSimpleName: SimpleName = asSimpleName
  }

  /** A term name (value/method name) */
  sealed abstract class TermName extends Name {
    override def isTypeName: Boolean = false
    override def toTermName: TermName = this
    override def toTypeName: TypeName = TypeName(this)
    override def ++ (other: String): TermName
  }

  /** A type name (class/type name) */
  case class TypeName(underlying: TermName) extends Name {
    override def isTypeName: Boolean = true
    override def toTermName: TermName = underlying
    override def toTypeName: TypeName = this
    override def toString: String = underlying.toString
    override def firstPart: SimpleName = underlying.firstPart
    override def lastPart: SimpleName = underlying.lastPart
    override def asSimpleName: SimpleName = underlying.asSimpleName
    override def ++ (other: String): TypeName = TypeName(underlying ++ other)
  }

  /** A simple (unqualified) term name */
  class SimpleName private[Names] (private val chars: String) extends TermName {
    override def toString: String = chars
    override def firstPart: SimpleName = this
    override def lastPart: SimpleName = this
    override def asSimpleName: SimpleName = this
    override def ++ (other: String): SimpleName = termName(chars + other)

    /** Replace characters */
    def replace(from: Char, to: Char): SimpleName =
      termName(chars.replace(from, to))

    /** Check if starts with string at offset */
    def startsWith(str: String, start: Int): Boolean =
      chars.indexOf(str, start) == start
  }

  /** A derived name with additional info */
  class DerivedName(val underlying: TermName, val info: NameInfo) extends TermName {
    override def toString: String = info.mkString(underlying)
    override def firstPart: SimpleName = underlying.firstPart
    override def lastPart: SimpleName = underlying.lastPart
    override def asSimpleName: SimpleName = underlying.asSimpleName
    override def ++ (other: String): DerivedName =
      DerivedName(underlying, info) // Simplified
  }

  /** Name info for derived names */
  sealed trait NameInfo {
    def mkString(underlying: TermName): String
  }

  /** Qualified name info */
  case class QualifiedInfo(separator: String, name: SimpleName) extends NameInfo {
    def mkString(underlying: TermName): String = s"$underlying$separator$name"
  }

  /** Intern a simple term name */
  def termName(s: String): SimpleName = {
    nameTable.getOrElseUpdate(s, new SimpleName(s))
  }

  /** Intern a simple type name */
  def typeName(s: String): TypeName = TypeName(termName(s))

  /** Create an empty term name */
  val EmptyTermName: SimpleName = termName("")

  /** Create an empty type name */
  val EmptyTypeName: TypeName = typeName("")

  /** Extension to convert strings to names */
  extension (s: String) {
    def toTermName: TermName = termName(s)
    def toTypeName: TypeName = typeName(s)
  }

  /** Create a qualified name */
  def qualifiedName(prefix: TermName, selector: SimpleName, separator: String = "."): TermName =
    DerivedName(prefix, QualifiedInfo(separator, selector))
}

