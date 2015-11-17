package dotty.tools
package dotc
package core

import scala.io.Codec
import util.NameTransformer
import printing.{Showable, Texts, Printer}
import Texts.Text
import Decorators._
import Contexts.Context
import collection.IndexedSeqOptimized
import collection.generic.CanBuildFrom
import collection.mutable.{ Builder, StringBuilder }
import collection.immutable.WrappedString
import collection.generic.CanBuildFrom
import util.DotClass
//import annotation.volatile

object Names {

  /** A common class for things that can be turned into names.
   *  Instances are both names and strings, the latter via a decorator.
   */
  trait PreName extends Any with Showable {
    def toTypeName: TypeName
    def toTermName: TermName
  }

  /** A name is essentially a string, with three differences
   *  1. Names belong in one of two name spaces: they are type names or term names.
   *     Term names have a sub-category of "local" field names.
   *     The same string can correspond a name in each of the three namespaces.
   *  2. Names are hash-consed. Two names
   *     representing the same string in the same universe are always reference identical.
   *  3. Names are intended to be encoded strings. @see dotc.util.NameTransformer.
   *     The encoding will be applied when converting a string to a name.
   */
  abstract class Name extends DotClass
    with PreName
    with Seq[Char]
    with IndexedSeqOptimized[Char, Name] {

    /** A type for names of the same kind as this name */
    type ThisName <: Name

    /** The start index in the character array */
    val start: Int

    /** The length of the names */
    override val length: Int

    /** Is this name a type name? */
    def isTypeName: Boolean

    /** Is this name a term name? */
    def isTermName: Boolean

    /** This name converted to a type name */
    def toTypeName: TypeName

    /** This name converted to a term name */
    def toTermName: TermName

    /** This name downcasted to a type name */
    def asTypeName: TypeName

    /** This name downcasted to a term name */
    def asTermName: TermName

    /** Create a new name of same kind as this one, in the given
     *  basis, with `len` characters taken from `cs` starting at `offset`.
     */
    def fromChars(cs: Array[Char], offset: Int, len: Int): ThisName

    /** Create new name of same kind as this name and with same
     *  characters as given `name`.
     */
    def fromName(name: Name): ThisName = fromChars(chrs, name.start, name.length)

    /** Create new name of same kind as this name with characters from
     *  the given string
     */
    def fromString(str: String): ThisName = {
      val cs = str.toCharArray
      fromChars(cs, 0, cs.length)
    }

    override def toString =
      if (length == 0) "" else new String(chrs, start, length)

    def toText(printer: Printer): Text = printer.toText(this)

    /** Write to UTF8 representation of this name to given character array.
     *  Start copying to index `to`. Return index of next free byte in array.
     *  Array must have enough remaining space for all bytes
     *  (i.e. maximally 3*length bytes).
     */
    final def copyUTF8(bs: Array[Byte], offset: Int): Int = {
      val bytes = Codec.toUTF8(chrs, start, length)
      scala.compat.Platform.arraycopy(bytes, 0, bs, offset, bytes.length)
      offset + bytes.length
    }

    /** Replace \$op_name's by corresponding operator symbols. */
    def decode: Name =
      if (contains('$')) fromString(NameTransformer.decode(toString))
      else this

    /** Replace operator symbols by corresponding \$op_name's. */
    def encode: Name =
      if (dontEncode(toTermName)) this else NameTransformer.encode(this)

    /** A more efficient version of concatenation */
    def ++ (other: Name): ThisName = ++ (other.toString)

    def ++ (other: String): ThisName = {
      val s = toString + other
      fromChars(s.toCharArray, 0, s.length)
    }

    def replace(from: Char, to: Char): ThisName = {
      val cs = new Array[Char](length)
      Array.copy(chrs, start, cs, 0, length)
      for (i <- 0 until length) {
        if (cs(i) == from) cs(i) = to
      }
      fromChars(cs, 0, length)
    }

    def contains(ch: Char): Boolean = {
      var i = 0
      while (i < length && chrs(start + i) != ch) i += 1
      i < length
    }

    def firstChar = chrs(start)

    // ----- Collections integration -------------------------------------

    override protected[this] def thisCollection: WrappedString = new WrappedString(repr.toString)
    override protected[this] def toCollection(repr: Name): WrappedString = new WrappedString(repr.toString)

    override protected[this] def newBuilder: Builder[Char, Name] = unsupported("newBuilder")

    override def apply(index: Int): Char = chrs(start + index)

    override def slice(from: Int, until: Int): ThisName =
      fromChars(chrs, start + from, until - from)

    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

    override def seq = toCollection(this)
  }

  class TermName(val start: Int, val length: Int, @sharable private[Names] var next: TermName) extends Name {
    // `next` is @sharable because it is only modified in the synchronized block of termName.
    type ThisName = TermName
    def isTypeName = false
    def isTermName = true

    @sharable // because it is only modified in the synchronized block of toTypeName.
    @volatile private[this] var _typeName: TypeName = null

    def toTypeName: TypeName = {
      if (_typeName == null)
        synchronized {
          if (_typeName == null)
            _typeName = new TypeName(start, length, this)
        }
      _typeName
    }
    def toTermName = this
    def asTypeName = throw new ClassCastException(this + " is not a type name")
    def asTermName = this

    override def hashCode: Int = start

    override protected[this] def newBuilder: Builder[Char, Name] = termNameBuilder

    def fromChars(cs: Array[Char], offset: Int, len: Int): TermName = termName(cs, offset, len)
  }

  class TypeName(val start: Int, val length: Int, val toTermName: TermName) extends Name {
    type ThisName = TypeName
    def isTypeName = true
    def isTermName = false
    def toTypeName = this
    def asTypeName = this
    def asTermName = throw new ClassCastException(this + " is not a term name")

    override def hashCode: Int = -start

    override protected[this] def newBuilder: Builder[Char, Name] =
      termNameBuilder.mapResult(_.toTypeName)

    def fromChars(cs: Array[Char], offset: Int, len: Int): TypeName = typeName(cs, offset, len)
  }

  // Nametable

  private final val InitialHashSize = 0x8000
  private final val InitialNameSize = 0x20000
  private final val fillFactor = 0.7

  /** Memory to store all names sequentially. */
  @sharable // because it's only mutated in synchronized block of termName
  private[dotty] var chrs: Array[Char] = new Array[Char](InitialNameSize)

  /** The number of characters filled. */
  @sharable // because it's only mutated in synchronized block of termName
  private var nc = 0

  /** Hashtable for finding term names quickly. */
  @sharable // because it's only mutated in synchronized block of termName
  private var table = new Array[TermName](InitialHashSize)

  /** The number of defined names. */
  @sharable // because it's only mutated in synchronized block of termName
  private var size = 1

  /** The hash of a name made of from characters cs[offset..offset+len-1].  */
  private def hashValue(cs: Array[Char], offset: Int, len: Int): Int =
    if (len > 0)
      (len * (41 * 41 * 41) +
        cs(offset) * (41 * 41) +
        cs(offset + len - 1) * 41 +
        cs(offset + (len >> 1)))
    else 0

  /** Is (the ASCII representation of) name at given index equal to
   *  cs[offset..offset+len-1]?
   */
  private def equals(index: Int, cs: Array[Char], offset: Int, len: Int): Boolean = {
    var i = 0
    while ((i < len) && (chrs(index + i) == cs(offset + i)))
      i += 1
    i == len
  }

  /** Create a term name from the characters in cs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def termName(cs: Array[Char], offset: Int, len: Int): TermName = synchronized {
    util.Stats.record("termName")
    val h = hashValue(cs, offset, len) & (table.size - 1)

    /** Make sure the capacity of the character array is at least `n` */
    def ensureCapacity(n: Int) =
      if (n > chrs.length) {
        val newchrs = new Array[Char](chrs.length * 2)
        chrs.copyToArray(newchrs)
        chrs = newchrs
      }

    /** Enter characters into chrs array. */
    def enterChars(): Unit = {
      ensureCapacity(nc + len)
      var i = 0
      while (i < len) {
        chrs(nc + i) = cs(offset + i)
        i += 1
      }
      nc += len
    }

    /** Rehash chain of names */
    def rehash(name: TermName): Unit =
      if (name != null) {
        val oldNext = name.next
        val h = hashValue(chrs, name.start, name.length) & (table.size - 1)
        name.next = table(h)
        table(h) = name
        rehash(oldNext)
      }

    /** Make sure the hash table is large enough for the given load factor */
    def incTableSize() = {
      size += 1
      if (size.toDouble / table.size > fillFactor) {
        val oldTable = table
        table = new Array[TermName](table.size * 2)
        for (i <- 0 until oldTable.size) rehash(oldTable(i))
      }
    }

    val next = table(h)
    var name = next
    while (name ne null) {
      if (name.length == len && equals(name.start, cs, offset, len))
        return name
      name = name.next
    }
    name = new TermName(nc, len, next)
    enterChars()
    table(h) = name
    incTableSize()
    name
  }

  /** Create a type name from the characters in cs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def typeName(cs: Array[Char], offset: Int, len: Int): TypeName =
    termName(cs, offset, len).toTypeName

  /** Create a term name from the UTF8 encoded bytes in bs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def termName(bs: Array[Byte], offset: Int, len: Int): TermName = {
    val chars = Codec.fromUTF8(bs, offset, len)
    termName(chars, 0, chars.length)
  }

  /** Create a type name from the UTF8 encoded bytes in bs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def typeName(bs: Array[Byte], offset: Int, len: Int): TypeName =
    termName(bs, offset, len).toTypeName

  /** Create a term name from a string, without encoding operators */
  def termName(s: String): TermName = termName(s.toCharArray, 0, s.length)

  /** Create a type name from a string, without encoding operators */
  def typeName(s: String): TypeName = typeName(s.toCharArray, 0, s.length)

  /** The term name represented by the empty string */
  val EmptyTermName = new TermName(-1, 0, null)

  table(0) = EmptyTermName

  /** The type name represented by the empty string */
  val EmptyTypeName = EmptyTermName.toTypeName

  // can't move CONSTRUCTOR/EMPTY_PACKAGE to `nme` because of bootstrap failures in `encode`.
  val CONSTRUCTOR = termName("<init>")
  val EMPTY_PACKAGE = termName("<empty>")

  val dontEncode = Set(CONSTRUCTOR, EMPTY_PACKAGE)

  def termNameBuilder: Builder[Char, TermName] =
    StringBuilder.newBuilder.mapResult(termName)

  implicit val nameCanBuildFrom: CanBuildFrom[Name, Char, Name] = new CanBuildFrom[Name, Char, Name] {
    def apply(from: Name): Builder[Char, Name] =
      StringBuilder.newBuilder.mapResult(s => from.fromChars(s.toCharArray, 0, s.length))
    def apply(): Builder[Char, Name] = termNameBuilder
  }

  implicit val NameOrdering: Ordering[Name] = new Ordering[Name] {
    def compare(x: Name, y: Name): Int = {
      if (x.isTermName && y.isTypeName) 1
      else if (x.isTypeName && y.isTermName) -1
      else if (x eq y) 0
      else {
        val until = x.length min y.length
        var i = 0

        while (i < until && x(i) == y(i)) i = i + 1

        if (i < until) {
          if (x(i) < y(i)) -1
          else /*(x(i) > y(i))*/ 1
        } else {
          x.length - y.length
        }
      }
    }
  }
}
