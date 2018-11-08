
object Names {
  trait PreName extends Any

  abstract class Name extends PreName

  /** Names for terms, can be simple or derived */
  abstract class TermName extends Name

  /** A simple name is essentially an interned string */
  final class SimpleName(val start: Int, val length: Int, private[Names] var next: SimpleName) extends TermName {
    /** A slice of this name making up the characters between `from` and `until` (exclusive) */
    def slice(from: Int, end: Int): SimpleName = {
      assert(0 <= from && from <= end && end <= length)
      termName(chrs, start + from, end - from)
    }

    def replace(from: Char, to: Char): SimpleName = {
      val cs = new Array[Char](length)
      Array.copy(chrs, start, cs, 0, length)
      for (i <- 0 until length) {
        if (cs(i) == from) cs(i) = to
      }
      termName(cs, 0, length)
    }
  }

  abstract class TypeName(val toTermName: TermName) extends Name

  // Nametable

  private final val InitialHashSize = 0x8000
  private final val InitialNameSize = 0x20000
  private final val fillFactor = 0.7

  /** Memory to store all names sequentially. */
   // because it's only mutated in synchronized block of termName
  var chrs: Array[Char] = new Array[Char](InitialNameSize)

  /** The number of characters filled. */
   // because it's only mutated in synchronized block of termName
  private[this] var nc = 0

  /** Hashtable for finding term names quickly. */
   // because it's only mutated in synchronized block of termName
  private[this] var table = new Array[SimpleName](InitialHashSize)

  /** The number of defined names. */
   // because it's only mutated in synchronized block of termName
  private[this] var size = 1

  /** The hash of a name made of from characters cs[offset..offset+len-1].  */
  private def hashValue(cs: Array[Char], offset: Int, len: Int): Int = {
    var i = offset
    var hash = 0
    while (i < len + offset) {
      hash = 31 * hash + cs(i)
      i += 1
    }
    hash
  }

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
  def termName(cs: Array[Char], offset: Int, len: Int): SimpleName = synchronized {
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
    def rehash(name: SimpleName): Unit =
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
        table = new Array[SimpleName](table.size * 2)
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
    name = new SimpleName(nc, len, next)
    enterChars()
    table(h) = name
    incTableSize()
    name
  }

  /** Create a term name from the UTF8 encoded bytes in bs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def termName(bs: Array[Byte], offset: Int, len: Int): SimpleName = {
    termName(bs, 0, bs.length)
  }

  /** Create a term name from a string, without encoding operators */
  def termName(s: String): SimpleName = termName(s.toCharArray, 0, s.length)

  table(0) = new SimpleName(-1, 0, null)
}

object Dummy {
  val a = b    // error
  val b = 3
}