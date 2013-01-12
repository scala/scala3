package dotty.tools.dotc
package core

import scala.io.Codec
import util.NameTransformer
import Periods._
import Decorators._

object Names {

  /** A name is essentially a string, with three differences
   *  1. Names belong in one of two universes: they are type names or term names.
   *     The same string can correspond both to a type name and to a term name.
   *  2. In each universe, names are hash-consed per basis. Two names
   *     representing the same string in the same basis are always reference identical.
   *  3. Names are intended to be encoded strings. @see dotc.util.NameTransformer.
   *     The encoding will be applied when converting a string to a name.
   */
  abstract class Name {

    /** The basis in which this name is stored */
    val basis: NameTable

    /** The start index in the character array */
    val start: Int

    /** The length of the names */
    val length: Int

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

    /** This name in the given basis */
    def in(basis: NameTable) =
      if (this.basis eq basis) this
      else newName(basis, this.basis.chrs, start, length)

    /** Create a new name of same kind as this one, in the given
     *  basis, with `len` characters taken from `cs` starting at `offset`.
     */
    protected def newName(basis: NameTable, cs: Array[Char], offset: Int, len: Int): Name

    /** A dummy equals method to catch all comparisons of names
     *  to other entities (e.g. strings).
     *  One always should use the ==(Name) method instead.
     */
    final override def equals(that: Any): Boolean = ??? // do not implement

    /** The only authorized == method on names */
    def == (that: Name): Boolean = (
      (this eq that)
      ||
      (this.basis ne that.basis) &&
      (this == (that in this.basis))
    )

    override def toString = new String(basis.chrs, start, length)

    /** Write to UTF8 representation of this name to given character array.
     *  Start copying to index `to`. Return index of next free byte in array.
     *  Array must have enough remaining space for all bytes
     *  (i.e. maximally 3*length bytes).
     */
    final def copyUTF8(bs: Array[Byte], offset: Int): Int = {
      val bytes = Codec.toUTF8(basis.chrs, start, length)
      scala.compat.Platform.arraycopy(bytes, 0, bs, offset, bytes.length)
      offset + bytes.length
    }

    /** Convert to string replacing operator symbols by corresponding \$op_name. */
    def decode: String = NameTransformer.decode(toString)

    /** The last phase id where symbols with this name can be created. */
    def lastIntroPhaseId: PhaseId = ???
  }

  class TermName(val basis: NameTable, val start: Int, val length: Int, val next: TermName) extends Name {
    def isTypeName = false
    def isTermName = true
    lazy val toTypeName: TypeName = new TypeName(basis, start, length, this)
    def toTermName = this
    def asTypeName = throw new ClassCastException(this+" is not a type name")
    def asTermName = this

    protected def newName(basis: NameTable, cs: Array[Char], offset: Int, len: Int): Name =
      basis.newTermName(cs, offset, len)
  }

  class TypeName(val basis: NameTable, val start: Int, val length: Int, val toTermName: TermName) extends Name {
    def isTypeName = true
    def isTermName = false
    def toTypeName = this
    def asTypeName = this
    def asTermName = throw new ClassCastException(this+" is not a term name")

    protected def newName(basis: NameTable, cs: Array[Char], offset: Int, len: Int): Name =
      basis.newTypeName(cs, offset, len)
  }

  class NameTable {

    private final val HASH_SIZE = 0x8000
    private final val HASH_MASK = 0x7FFF
    private final val NAME_SIZE = 0x20000
    final val nameDebug = false

    /** Memory to store all names sequentially. */
    private[Names] var chrs: Array[Char] = new Array[Char](NAME_SIZE)
    private var nc = 0

    /** Hashtable for finding term names quickly. */
    private val table = new Array[Names.TermName](HASH_SIZE)

    /** The hashcode of a name. */
    private def hashValue(cs: Array[Char], offset: Int, len: Int): Int =
      if (len > 0)
        (len * (41 * 41 * 41) +
          cs(offset) * (41 * 41) +
          cs(offset + len - 1) * 41 +
          cs(offset + (len >> 1)))
      else 0

    /**
     * Is (the ASCII representation of) name at given index equal to
     *  cs[offset..offset+len-1]?
     */
    private def equals(index: Int, cs: Array[Char], offset: Int, len: Int): Boolean = {
      var i = 0
      while ((i < len) && (chrs(index + i) == cs(offset + i)))
        i += 1;
      i == len
    }

    /** Enter characters into chrs array. */
    private def enterChars(cs: Array[Char], offset: Int, len: Int) {
      var i = 0
      while (i < len) {
        if (nc + i == chrs.length) {
          val newchrs = new Array[Char](chrs.length * 2)
          scala.compat.Platform.arraycopy(chrs, 0, newchrs, 0, chrs.length)
          chrs = newchrs
        }
        chrs(nc + i) = cs(offset + i)
        i += 1
      }
      if (len == 0) nc += 1
      else nc = nc + len
    }

    /**
     * Create a term name from the characters in cs[offset..offset+len-1].
     *  Assume they are already encoded.
     */
    def newTermName(cs: Array[Char], offset: Int, len: Int): TermName = /* sync if parallel */ {
      val h = hashValue(cs, offset, len) & HASH_MASK
      val next = table(h)
      var name = next
      while ((name ne null) && (name.length != len || !equals(name.start, cs, offset, len)))
        name = name.next

      if (name eq null) /* needs sync if parallel */ {
        name = new TermName(this, nc, len, next)
        enterChars(cs, offset, len)
        table(h) = name
        name
      }

      name
    }

    /**
     * Create a type name from the characters in cs[offset..offset+len-1].
     *  Assume they are already encoded.
     */
    def newTypeName(cs: Array[Char], offset: Int, len: Int): TypeName =
      newTermName(cs, offset, len).toTypeName

    /**
     * Create a term name from the UTF8 encoded bytes in bs[offset..offset+len-1].
     *  Assume they are already encoded.
     */
    def newTermName(bs: Array[Byte], offset: Int, len: Int): TermName = {
      val chars = Codec.fromUTF8(bs, offset, len)
      newTermName(chars, 0, chars.length)
    }

    /**
     * Create a type name from the UTF8 encoded bytes in bs[offset..offset+len-1].
     *  Assume they are already encoded.
     */
    def newTypeName(bs: Array[Byte], offset: Int, len: Int): TypeName =
      newTermName(bs, offset, len).toTypeName

    /** Create a term name from a string, encode if necessary*/
    def newTermName(s: String): TermName = {
      val es = NameTransformer.encode(s)
      newTermName(es.toCharArray, 0, es.length)
    }

    /** Create a type name from a string, encode if necessary */
    def newTypeName(s: String): TypeName = {
      val es = NameTransformer.encode(s)
      newTypeName(es.toCharArray, 0, es.length)
    }
  }

  object BootNameTable extends NameTable

  val EmptyTypeName = BootNameTable.newTypeName("")
  val EmptyTermName = BootNameTable.newTermName("")
}