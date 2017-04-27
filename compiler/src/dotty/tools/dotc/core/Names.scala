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
import collection.mutable.{ Builder, StringBuilder, AnyRefMap }
import collection.immutable.WrappedString
import collection.generic.CanBuildFrom
import util.{DotClass, SimpleMap}
import java.util.HashMap

//import annotation.volatile

object Names {
  import NameKinds._

  /** A common class for things that can be turned into names.
   *  Instances are both names and strings, the latter via a decorator.
   */
  trait PreName extends Any with Showable {
    def toTypeName: TypeName
    def toTermName: TermName
  }

  implicit def eqName: Eq[Name, Name] = Eq

  /** A name is essentially a string, with three differences
   *  1. Names belong in one of two name spaces: they are type names or term names.
   *     Term names have a sub-category of "local" field names.
   *     The same string can correspond a name in each of the three namespaces.
   *  2. Names are hash-consed. Two names
   *     representing the same string in the same universe are always reference identical.
   *  3. Names are intended to be encoded strings. @see dotc.util.NameTransformer.
   *     The encoding will be applied when converting a string to a name.
   */
  abstract class Name extends DotClass with PreName {

    /** A type for names of the same kind as this name */
    type ThisName <: Name

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

    def isSimple: Boolean
    def asSimpleName: SimpleTermName
    def toSimpleName: SimpleTermName
    def mangled: Name
    def mangledString: String = mangled.toString

    def rewrite(f: PartialFunction[Name, Name]): ThisName
    def collect[T](f: PartialFunction[Name, T]): Option[T]
    def mapLast(f: SimpleTermName => SimpleTermName): ThisName
    def mapParts(f: SimpleTermName => SimpleTermName): ThisName

    /** A name in the same (term or type) namespace as this name and
     *  with same characters as given `name`.
     */
    def likeSpaced(name: Name): ThisName

    def derived(info: NameInfo): ThisName
    def derived(kind: ClassifiedNameKind): ThisName = derived(kind.info)
    def exclude(kind: NameKind): ThisName
    def is(kind: NameKind): Boolean
    def debugString: String

    def toText(printer: Printer): Text = printer.toText(this)

    /** Replace \$op_name's by corresponding operator symbols. */
    def decode: Name

    /** Replace operator symbols by corresponding \$op_name's. */
    def encode: Name

    def firstPart: SimpleTermName
    def lastPart: SimpleTermName

    /** A more efficient version of concatenation */
    def ++ (other: Name): ThisName = ++ (other.toString)
    def ++ (other: String): ThisName = mapLast(n => termName(n.toString + other))
    def replace(from: Char, to: Char): ThisName = mapParts(_.replace(from, to))

    def isEmpty: Boolean

    def startsWith(str: String): Boolean = firstPart.startsWith(str)
    def endsWith(str: String): Boolean = lastPart.endsWith(str)

    override def hashCode = System.identityHashCode(this)
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  abstract class TermName extends Name {
    type ThisName = TermName
    def isTypeName = false
    def isTermName = true
    def toTermName = this
    def asTypeName = throw new ClassCastException(this + " is not a type name")
    def asTermName = this

    @sharable // because it is only modified in the synchronized block of toTypeName.
    @volatile private[this] var _typeName: TypeName = null

    def toTypeName: TypeName = {
      if (_typeName == null)
        synchronized {
          if (_typeName == null)
            _typeName = new TypeName(this)
        }
      _typeName
    }

    def likeSpaced(name: Name): TermName = name.toTermName

    def info: NameInfo = SimpleTermNameKind.info
    def underlying: TermName = unsupported("underlying")

    @sharable // because of synchronized block in `and`
    private var derivedNames: AnyRef /* SimpleMap | j.u.HashMap */ =
      SimpleMap.Empty[NameInfo]

    private def getDerived(info: NameInfo): DerivedTermName /* | Null */= derivedNames match {
      case derivedNames: SimpleMap[NameInfo, DerivedTermName] @unchecked =>
        derivedNames(info)
      case derivedNames: HashMap[NameInfo, DerivedTermName] @unchecked =>
        derivedNames.get(info)
    }

    private def putDerived(info: NameInfo, name: DerivedTermName): name.type = {
      derivedNames match {
        case derivedNames: SimpleMap[NameInfo, DerivedTermName] @unchecked =>
          if (derivedNames.size < 4)
            this.derivedNames = derivedNames.updated(info, name)
          else {
            val newMap = new HashMap[NameInfo, DerivedTermName]
            derivedNames.foreachBinding(newMap.put(_, _))
            newMap.put(info, name)
            this.derivedNames = newMap
          }
        case derivedNames: HashMap[NameInfo, DerivedTermName] @unchecked =>
          derivedNames.put(info, name)
      }
      name
    }

    private def add(info: NameInfo): TermName = synchronized {
      getDerived(info) match {
        case null        => putDerived(info, new DerivedTermName(this, info))
        case derivedName => derivedName
      }
    }

    private def rewrap(underlying: TermName) =
      if (underlying eq this.underlying) this else underlying.add(info)

    /** Return derived name with given `info` and the current
     *  name as underlying name.
     */
    def derived(info: NameInfo): TermName = {
      val thisKind = this.info.kind
      val thatKind = info.kind
      if (thisKind.tag < thatKind.tag || thatKind.definesNewName) add(info)
      else if (thisKind.tag > thatKind.tag) rewrap(underlying.derived(info))
      else {
        assert(info == this.info)
        this
      }
    }

    def exclude(kind: NameKind): TermName = {
      val thisKind = this.info.kind
      if (thisKind.tag < kind.tag || thisKind.definesNewName) this
      else if (thisKind.tag > kind.tag) rewrap(underlying.exclude(kind))
      else underlying
    }

    def is(kind: NameKind): Boolean = {
      val thisKind = this.info.kind
      thisKind == kind ||
      !thisKind.definesNewName && thisKind.tag > kind.tag && underlying.is(kind)
    }
  }

  class SimpleTermName(val start: Int, val length: Int, @sharable private[Names] var next: SimpleTermName) extends TermName {
    // `next` is @sharable because it is only modified in the synchronized block of termName.

    def apply(n: Int) = chrs(start + n)

    def exists(p: Char => Boolean): Boolean = {
      var i = 0
      while (i < length && !p(chrs(start + i))) i += 1
      i < length
    }

    def forall(p: Char => Boolean) = !exists(!p(_))

    def contains(ch: Char): Boolean = {
      var i = 0
      while (i < length && chrs(start + i) != ch) i += 1
      i < length
    }

    def isEmpty = length == 0

    override def startsWith(str: String): Boolean = {
      var i = 0
      while (i < str.length && i < length && apply(i) == str(i)) i += 1
      i == str.length
    }

    override def endsWith(str: String): Boolean = {
      var i = 1
      while (i <= str.length && i <= length && apply(length - i) == str(str.length - i)) i += 1
      i > str.length
    }

    def lastIndexOf(ch: Char, start: Int = length - 1): Int = {
      var i = start
      while (i >= 0 && apply(i) != ch) i -= 1
      i
    }

    def lastIndexOfSlice(str: String): Int = toString.lastIndexOfSlice(str)

    override def replace(from: Char, to: Char): SimpleTermName = {
      val cs = new Array[Char](length)
      Array.copy(chrs, start, cs, 0, length)
      for (i <- 0 until length) {
        if (cs(i) == from) cs(i) = to
      }
      termName(cs, 0, length)
    }

    def slice(from: Int, until: Int): SimpleTermName = {
      assert(0 <= from && from <= until && until <= length)
      termName(chrs, start + from, until - from)
    }

    def drop(n: Int) = slice(n, length)
    def take(n: Int) = slice(0, n)
    def dropRight(n: Int) = slice(0, length - n)
    def takeRight(n: Int) = slice(length - n, length)

    def head = apply(0)
    def last = apply(length - 1)

    def isSimple = true
    def asSimpleName = this
    def toSimpleName = this
    def mangled = this

    def rewrite(f: PartialFunction[Name, Name]): ThisName =
      if (f.isDefinedAt(this)) likeSpaced(f(this)) else this
    def collect[T](f: PartialFunction[Name, T]): Option[T] = f.lift(this)
    def mapLast(f: SimpleTermName => SimpleTermName) = f(this)
    def mapParts(f: SimpleTermName => SimpleTermName) = f(this)

    def encode: SimpleTermName =
      if (dontEncode(toTermName)) this else NameTransformer.encode(this)

    /** Replace \$op_name's by corresponding operator symbols. */
    def decode: SimpleTermName =
      if (contains('$')) termName(NameTransformer.decode(toString)) else this

    def firstPart = this
    def lastPart = this

    override def hashCode: Int = start

    override def toString =
      if (length == 0) ""
      else new String(chrs, start, length)

    def debugString: String = toString
  }

  class TypeName(val toTermName: TermName) extends Name {

    def isEmpty = toTermName.isEmpty

    def encode   = toTermName.encode.toTypeName
    def decode   = toTermName.decode.toTypeName
    def firstPart = toTermName.firstPart
    def lastPart = toTermName.lastPart

    type ThisName = TypeName
    def isTypeName = true
    def isTermName = false
    def toTypeName = this
    def asTypeName = this
    def asTermName = throw new ClassCastException(this + " is not a term name")

    def isSimple = toTermName.isSimple
    def asSimpleName = toTermName.asSimpleName
    def toSimpleName = toTermName.toSimpleName
    def mangled = toTermName.toSimpleName.toTypeName

    def rewrite(f: PartialFunction[Name, Name]): ThisName = toTermName.rewrite(f).toTypeName
    def collect[T](f: PartialFunction[Name, T]): Option[T] = toTermName.collect(f)
    def mapLast(f: SimpleTermName => SimpleTermName) = toTermName.mapLast(f).toTypeName
    def mapParts(f: SimpleTermName => SimpleTermName) = toTermName.mapParts(f).toTypeName

    def likeSpaced(name: Name): TypeName = name.toTypeName

    def derived(info: NameInfo): TypeName = toTermName.derived(info).toTypeName
    def exclude(kind: NameKind): TypeName = toTermName.exclude(kind).toTypeName
    def is(kind: NameKind) = toTermName.is(kind)

    override def toString = toTermName.toString
    override def debugString = toTermName.debugString + "/T"
  }

  /** A term name that's derived from an `underlying` name and that
   *  adds `info` to it.
   */
  case class DerivedTermName(override val underlying: TermName, override val info: NameInfo)
  extends TermName {
    def isEmpty = false
    def encode: Name = underlying.encode.derived(info.map(_.encode))
    def decode: Name = underlying.decode.derived(info.map(_.decode))
    def firstPart = underlying.firstPart
    def lastPart = info match {
      case qual: QualifiedInfo => qual.name
      case _ => underlying.lastPart
    }
    override def toString = info.mkString(underlying)
    override def debugString = s"${underlying.debugString}[$info]"

    def isSimple = false
    def asSimpleName = throw new UnsupportedOperationException(s"$debugString is not a simple name")

    private[this] var simpleName: SimpleTermName = null
    def toSimpleName = {
      if (simpleName == null) simpleName = termName(toString)
      simpleName
    }
    def mangled = toSimpleName

    def rewrite(f: PartialFunction[Name, Name]): ThisName =
      if (f.isDefinedAt(this)) likeSpaced(f(this))
      else info match {
        case qual: QualifiedInfo => this
        case _ => underlying.rewrite(f).derived(info)
      }

    def collect[T](f: PartialFunction[Name, T]): Option[T] =
      if (f.isDefinedAt(this)) Some(f(this))
      else info match {
        case qual: QualifiedInfo => None
        case _ => underlying.collect(f)
      }

    def mapLast(f: SimpleTermName => SimpleTermName): ThisName =
      info match {
        case qual: QualifiedInfo => underlying.derived(qual.map(f))
        case _ => underlying.mapLast(f).derived(info)
      }

    def mapParts(f: SimpleTermName => SimpleTermName): ThisName =
      info match {
        case qual: QualifiedInfo => underlying.mapParts(f).derived(qual.map(f))
        case _ => underlying.mapParts(f).derived(info)
      }
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
  private var table = new Array[SimpleTermName](InitialHashSize)

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
  def termName(cs: Array[Char], offset: Int, len: Int): SimpleTermName = synchronized {
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
    def rehash(name: SimpleTermName): Unit =
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
        table = new Array[SimpleTermName](table.size * 2)
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
    name = new SimpleTermName(nc, len, next)
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
  def termName(bs: Array[Byte], offset: Int, len: Int): SimpleTermName = {
    val chars = Codec.fromUTF8(bs, offset, len)
    termName(chars, 0, chars.length)
  }

  /** Create a type name from the UTF8 encoded bytes in bs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def typeName(bs: Array[Byte], offset: Int, len: Int): TypeName =
    termName(bs, offset, len).toTypeName

  /** Create a term name from a string, without encoding operators */
  def termName(s: String): SimpleTermName = termName(s.toCharArray, 0, s.length)

  /** Create a type name from a string, without encoding operators */
  def typeName(s: String): TypeName = typeName(s.toCharArray, 0, s.length)

  table(0) = new SimpleTermName(-1, 0, null)

  /** The term name represented by the empty string */
  val EmptyTermName: TermName = table(0)

  /** The type name represented by the empty string */
  val EmptyTypeName = EmptyTermName.toTypeName

  // can't move CONSTRUCTOR/EMPTY_PACKAGE to `nme` because of bootstrap failures in `encode`.
  val CONSTRUCTOR: TermName = termName("<init>")
  val STATIC_CONSTRUCTOR: TermName = termName("<clinit>")
  val EMPTY_PACKAGE: TermName = termName("<empty>")
  val REFINEMENT: TermName = termName("<refinement>")

  val dontEncode = Set(CONSTRUCTOR, EMPTY_PACKAGE, REFINEMENT)

  implicit val NameOrdering: Ordering[Name] = new Ordering[Name] {
    private def compareInfos(x: NameInfo, y: NameInfo): Int =
      if (x.kind.tag != y.kind.tag) x.kind.tag - y.kind.tag
      else x match {
        case x: QualifiedInfo =>
          y match {
            case y: QualifiedInfo =>
              compareSimpleNames(x.name, y.name)
          }
        case x: NumberedInfo =>
          y match {
            case y: NumberedInfo =>
              x.num - y.num
          }
        case _ =>
          assert(x == y)
          0
      }
    private def compareSimpleNames(x: SimpleTermName, y: SimpleTermName): Int = {
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
    private def compareTermNames(x: TermName, y: TermName): Int = x match {
      case x: SimpleTermName =>
        y match {
          case y: SimpleTermName => compareSimpleNames(x, y)
          case _ => -1
        }
      case DerivedTermName(xPre, xInfo) =>
        y match {
          case DerivedTermName(yPre, yInfo) =>
            val s = compareInfos(xInfo, yInfo)
            if (s == 0) compareTermNames(xPre, yPre) else s
          case _ => 1
        }
    }
    def compare(x: Name, y: Name): Int = {
      if (x.isTermName && y.isTypeName) 1
      else if (x.isTypeName && y.isTermName) -1
      else if (x eq y) 0
      else compareTermNames(x.toTermName, y.toTermName)
    }
  }
}
