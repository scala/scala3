package dotty.tools
package dotc
package core

import scala.io.Codec
import util.NameTransformer
import printing.{Showable, Texts, Printer}
import Texts.Text
import StdNames.str
import config.Config
import util.LinearMap

import scala.annotation.internal.sharable

object Names {
  import NameKinds.*

  /** Things that can be turned into names with `toTermName` and `toTypeName`.
   *  Decorators implements these as extension methods for strings.
   */
  type PreName = Name | String

  /** A common type of Name and Symbol */
  type Designator = Name | Symbols.Symbol

  /** A name is either a term name or a type name. Term names can be simple
   *  or derived. A simple term name is essentially an interned string stored
   *  in a name table. A derived term name adds a tag, and possibly a number
   *  or a further simple name to some other name.
   */
  abstract class Name extends Showable derives CanEqual {

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

    /** This name downcasted to a simple term name */
    def asSimpleName: SimpleName

    /** This name converted to a simple term name */
    def toSimpleName: SimpleName

    /** This name converted to a simple term name and in addition
     *  with all symbolic operator characters expanded.
     */
    def mangled: ThisName

    /** Convert to string after mangling */
    def mangledString: String

    /** Apply rewrite rule given by `f` to some part of this name, skipping and rewrapping
     *  other decorators.
     *  Stops at DerivedNames with infos of kind QualifiedInfo.
     *  If `f` does not apply to any part, return name unchanged.
     */
    def replace(f: PartialFunction[Name, Name]): ThisName

    /** Same as replace, but does not stop at DerivedNames with infos of kind QualifiedInfo. */
    def replaceDeep(f: PartialFunction[Name, Name]): ThisName =
      replace(f.orElse {
        case DerivedName(underlying, info: QualifiedInfo) =>
          underlying.replaceDeep(f).derived(info)
      })

    /** If partial function `f` is defined for some part of this name, apply it
     *  in a Some, otherwise None.
     *  Stops at derived names whose kind has `definesNewName = true`.
     */
    def collect[T](f: PartialFunction[Name, T]): Option[T]

    /** Apply `f` to last simple term name making up this name */
    def mapLast(f: SimpleName => SimpleName): ThisName

    /** Apply `f` to all simple term names making up this name */
    def mapParts(f: SimpleName => SimpleName): ThisName

    /** A name in the same (term or type) namespace as this name and
     *  with same characters as given `name`.
     */
    def likeSpaced(name: Name): ThisName

    /** A derived name consisting of this name and the added info, unless it is
     *  already present in this name.
     *  @pre This name does not have a different info of the same kind as `info`.
     */
    def derived(info: NameInfo): ThisName

    /** A derived name consisting of this name and the info of `kind` */
    def derived(kind: ClassifiedNameKind): ThisName = derived(kind.info)

    /** This name without any info of the given `kind`. Excepted, as always,
     *  is the underlying name part of a qualified name.
     */
    def exclude(kind: NameKind): ThisName

    /** Does this name contain an info of the given kind? Excepted, as always,
     *  is the underlying name part of a qualified name.
     */
    def is(kind: NameKind): Boolean

    /** A string showing the internal structure of this name. By contrast, `toString`
     *  shows the name after conversion to a simple name.
     */
    def debugString: String

    /** Convert name to text via `printer`. */
    def toText(printer: Printer): Text = printer.toText(this)

    /** Replace operator expansions by corresponding operator symbols. */
    def decode: ThisName

    /** Replace operator symbols by corresponding operator expansions */
    def encode: ThisName

    /** The first part of this (possible qualified) name */
    def firstPart: SimpleName

    /** The last part of this (possible qualified) name */
    def lastPart: SimpleName

    /** Append `other` to the last part of this name */
    def ++ (other: Name): ThisName = ++ (other.toString)
    def ++ (other: String): ThisName = mapLast(n => termName(n.toString + other))

    /** Replace all occurrences of `from` to `to` in this name */
    def replace(from: Char, to: Char): ThisName = mapParts(_.replace(from, to))

    /** Is this name empty? */
    def isEmpty: Boolean

    /** Does (the first part of) this name starting at index `start` starts with `str`? */
    def startsWith(str: String, start: Int = 0): Boolean = firstPart.startsWith(str, start)

    /** Does (the last part of) this name end with `str`? */
    def endsWith(suffix: String): Boolean = lastPart.endsWith(suffix)

    def endsWith(suffix: SimpleName): Boolean = lastPart.endsWith(suffix)

    override def hashCode: Int = System.identityHashCode(this)
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  }

  /** Names for terms, can be simple or derived */
  abstract class TermName extends Name {
    type ThisName = TermName

    override def isTypeName: Boolean = false
    override def isTermName: Boolean = true
    override def toTermName: TermName = this
    override def asTypeName: Nothing = throw new ClassCastException(s"$this is not a type name")
    override def asTermName: TermName = this

    @sharable // because it is only modified in the synchronized block of toTypeName.
    private var myTypeName: TypeName | Null = null
      // Note: no @volatile needed since type names are immutable and therefore safely published

    override def toTypeName: TypeName =
      if myTypeName == null then
        synchronized {
          if myTypeName == null then myTypeName = new TypeName(this)
        }
      myTypeName.nn

    override def likeSpaced(name: Name): TermName = name.toTermName

    def info: NameInfo = SimpleNameKind.info
    def underlying: TermName = unsupported("underlying")

    @sharable // because of synchronized block in `add`
    private var derivedNames: LinearMap[NameInfo, DerivedName] = LinearMap.empty

    private def add(info: NameInfo): TermName = synchronized {
      val dnOpt = derivedNames.lookup(info)
      dnOpt match
        case null =>
          val derivedName = new DerivedName(this, info)
          derivedNames = derivedNames.updated(info, derivedName)
          derivedName
        case _ => dnOpt
    }

    private def rewrap(underlying: TermName) =
      if (underlying eq this.underlying) this else underlying.add(info)

    override def derived(info: NameInfo): TermName = {
      val thisKind = this.info.kind
      val thatKind = info.kind
      if (thisKind.tag < thatKind.tag || thatKind.definesNewName) add(info)
      else if (thisKind.tag > thatKind.tag) rewrap(underlying.derived(info))
      else {
        assert(info == this.info)
        this
      }
    }

    /** Is it impossible that names of kind `kind` also qualify as names of kind `shadowed`? */
    private def shadows(kind: NameKind, shadowed: NameKind): Boolean =
      kind.tag < shadowed.tag ||
      kind.definesQualifiedName ||
      kind.definesNewName && !shadowed.definesQualifiedName

    override def exclude(kind: NameKind): TermName = {
      val thisKind = this.info.kind
      if (shadows(thisKind, kind)) this
      else if (thisKind.tag > kind.tag) rewrap(underlying.exclude(kind))
      else underlying
    }

    override def is(kind: NameKind): Boolean = {
      val thisKind = this.info.kind
      thisKind == kind ||
      !shadows(thisKind, kind) && underlying.is(kind)
    }

    @sharable // because it's just a cache for performance
    private var myMangledString: String | Null = null

    @sharable // because it's just a cache for performance
    private var myMangled: Name | Null = null

    protected[Names] def mangle: ThisName

    final def mangled: ThisName = {
      if (myMangled == null) myMangled = mangle
      myMangled.asInstanceOf[ThisName]
    }

    final def mangledString: String = {
      if (myMangledString == null)
        myMangledString = qualToString(_.mangledString, _.mangled.toString)
      myMangledString.nn
    }

    /** If this a qualified name, split it into underlying, last part, and separator
     *  Otherwise return an empty name, the name itself, and "")
     */
    def split: (TermName, TermName, String)

    /** Convert to string as follows. If this is a qualified name
     *  `<first> <sep> <last>`, the sanitized version of `f1(<first>) <sep> f2(<last>)`.
     *  Otherwise `f2` applied to this name.
     */
    def qualToString(f1: TermName => String, f2: TermName => String): String = {
      val (first, last, sep) = split
      if (first.isEmpty) f2(last) else str.sanitize(f1(first) + sep + f2(last))
    }

    protected def computeToString: String

    @sharable private var myToString: String | Null = null

    override def toString: String =
      if myToString == null then myToString = computeToString
      myToString.nn

  }

  /** A simple name is essentially an interned string. It owns its `chars` array, so it and its
   *  characters are reclaimed by GC once unreferenced (issue #1584). Created only by the `NameTable`
   *  (constructor and `chars` are private), so each spelling has one canonical instance with
   *  immutable characters — the invariant that `eq` comparison, scope lookup, keyword recognition
   *  and the cached content `hashCode` rely on.
   */
  final class SimpleName private[Names] (private[Names] val chars: Array[Char]) extends TermName {

    /** The number of characters in this name. */
    val length: Int = chars.length

  /** The n'th character */
    def apply(n: Int): Char = chars(n)

    /** A character in this name satisfies predicate `p` */
    def exists(p: Char => Boolean): Boolean = {
      var i = 0
      while (i < length && !p(chars(i))) i += 1
      i < length
    }

    /** All characters in this name satisfy predicate `p` */
    def forall(p: Char => Boolean): Boolean = !exists(!p(_))

    /** The name contains given character `ch` */
    def contains(ch: Char): Boolean = {
      var i = 0
      while (i < length && chars(i) != ch) i += 1
      i < length
    }

    /** The index of the last occurrence of `ch` in this name which is at most
     *  `start`.
     */
    def lastIndexOf(ch: Char, start: Int = length - 1): Int = {
      var i = start
      while (i >= 0 && apply(i) != ch) i -= 1
      i
    }

    /** The index of the last occurrence of `str` in this name */
    def lastIndexOfSlice(str: String): Int = toString.lastIndexOfSlice(str)

    /** A slice of this name making up the characters between `from` and `until` (exclusive) */
    def slice(from: Int, end: Int): SimpleName = {
      assert(0 <= from && from <= end && end <= length)
      termName(chars, from, end - from)
    }

    def drop(n: Int): SimpleName = slice(n, length)
    def take(n: Int): SimpleName = slice(0, n)
    def dropRight(n: Int): SimpleName = slice(0, length - n)
    def takeRight(n: Int): SimpleName = slice(length - n, length)

    /** Same as slice, but as a string */
    def sliceToString(from: Int, end: Int): String =
      if (end <= from) "" else new String(chars, from, end - from)

    def head: Char = apply(0)
    def last: Char = apply(length - 1)

    /** Copy character slice (from until end) to character array starting at `dstStart`.
     *  @pre Destination must have enough space to hold all characters of this name.
     */
    def getChars(from: Int, end: Int, dst: Array[Char], dstStart: Int): Unit =
      assert(0 <= from && from <= end && end <= length)
      Array.copy(chars, from, dst, dstStart, end - from)

    /** UTF8 encoding of the characters, as a fresh array (so consumers don't alias `chars`). */
    private[dotc] def toUTF8: Array[Byte] = Codec.toUTF8(chars, 0, length)

    override def asSimpleName: SimpleName = this
    override def toSimpleName: SimpleName = this
    override final def mangle: SimpleName = encode

    override def replace(f: PartialFunction[Name, Name]): ThisName =
      if (f.isDefinedAt(this)) likeSpaced(f(this)) else this
    override def collect[T](f: PartialFunction[Name, T]): Option[T] = f.lift(this)
    override def mapLast(f: SimpleName => SimpleName): SimpleName = f(this)
    override def mapParts(f: SimpleName => SimpleName): SimpleName = f(this)
    override def split: (TermName, SimpleName, String) = (EmptyTermName, this, "")

    override def encode: SimpleName = {
      val dontEncode =
        this == StdNames.nme.CONSTRUCTOR || this == StdNames.nme.STATIC_CONSTRUCTOR
      if (dontEncode) this else NameTransformer.encode(this)
    }

    override def decode: SimpleName = NameTransformer.decode(this)

    override def isEmpty: Boolean = length == 0

    override def startsWith(str: String, start: Int): Boolean = {
      var i = 0
      while (i < str.length && start + i < length && apply(start + i) == str(i)) i += 1
      i == str.length
    }

    override def endsWith(suffix: String): Boolean =
      var i = 1
      while i <= suffix.length && i <= length && apply(length - i) == suffix(suffix.length - i) do i += 1
      i > suffix.length

    override def endsWith(suffix: SimpleName): Boolean =
      var i = 1
      while i <= suffix.length && i <= length && apply(length - i) == suffix(suffix.length - i) do i += 1
      i > suffix.length

    override def replace(from: Char, to: Char): SimpleName = {
      val cs = chars.clone()
      for (i <- 0 until length)
        if (cs(i) == from) cs(i) = to
      termName(cs, 0, length)
    }

    override def firstPart: SimpleName = this
    override def lastPart: SimpleName = this

    // A cached content hash. It must be content-based, not identity: a name can be collected and
    // re-interned as a fresh object, and an identity hash would then make name-keyed orderings —
    // hence generated names and pickled output — vary across runs (issue #1584). Equality stays
    // `eq`; interning keeps the two consistent (one instance per spelling).
    override val hashCode: Int = hashValue(chars, 0, length)

    protected def computeToString: String =
      if (length == 0) ""
      else new String(chars)

    def debugString: String = toString
  }

  final class TypeName(val toTermName: TermName) extends Name {

    type ThisName = TypeName

    override def isTypeName: Boolean = true
    override def isTermName: Boolean = false
    override def toTypeName: TypeName = this
    override def asTypeName: TypeName = this
    override def asTermName: Nothing = throw new ClassCastException(s"$this is not a term name")

    override def asSimpleName: SimpleName = toTermName.asSimpleName
    override def toSimpleName: SimpleName = toTermName.toSimpleName
    override def mangled: TypeName = toTermName.mangled.toTypeName
    override def mangledString: String = toTermName.mangledString

    override def replace(f: PartialFunction[Name, Name]): ThisName = toTermName.replace(f).toTypeName
    override def collect[T](f: PartialFunction[Name, T]): Option[T] = toTermName.collect(f)
    override def mapLast(f: SimpleName => SimpleName): TypeName = toTermName.mapLast(f).toTypeName
    override def mapParts(f: SimpleName => SimpleName): TypeName = toTermName.mapParts(f).toTypeName

    override def likeSpaced(name: Name): TypeName = name.toTypeName

    override def derived(info: NameInfo): TypeName = toTermName.derived(info).toTypeName
    override def exclude(kind: NameKind): TypeName = toTermName.exclude(kind).toTypeName
    override def is(kind: NameKind): Boolean = toTermName.is(kind)

    override def isEmpty: Boolean = toTermName.isEmpty

    override def encode: TypeName   = toTermName.encode.toTypeName
    override def decode: TypeName   = toTermName.decode.toTypeName
    override def firstPart: SimpleName = toTermName.firstPart
    override def lastPart: SimpleName = toTermName.lastPart

    // Content-based, like `SimpleName.hashCode`: a type name and its term name share characters,
    // so this is deterministic and stable across re-interning under weak interning (issue #1584).
    override def hashCode: Int = toTermName.hashCode

    override def toString: String = toTermName.toString
    override def debugString: String = toTermName.debugString + "/T"
  }

  /** A term name that's derived from an `underlying` name and that
   *  adds `info` to it.
   */
  final case class DerivedName(override val underlying: TermName, override val info: NameInfo)
  extends TermName {

    override def asSimpleName: Nothing = throw new UnsupportedOperationException(s"$debugString is not a simple name")

    override def toSimpleName: SimpleName = termName(toString)
    override final def mangle: SimpleName = encode.toSimpleName

    override def replace(f: PartialFunction[Name, Name]): ThisName =
      if (f.isDefinedAt(this)) likeSpaced(f(this))
      else info match {
        case qual: QualifiedInfo => this
        case _ => underlying.replace(f).derived(info)
      }

    override def collect[T](f: PartialFunction[Name, T]): Option[T] =
      if (f.isDefinedAt(this)) Some(f(this))
      else info match {
        case qual: QualifiedInfo => None
        case _ => underlying.collect(f)
      }

    override def mapLast(f: SimpleName => SimpleName): ThisName =
      info match {
        case qual: QualifiedInfo => underlying.derived(qual.map(f))
        case _ => underlying.mapLast(f).derived(info)
      }

    override def mapParts(f: SimpleName => SimpleName): ThisName =
      info match {
        case qual: QualifiedInfo => underlying.mapParts(f).derived(qual.map(f))
        case _ => underlying.mapParts(f).derived(info)
      }

    override def split: (TermName, TermName, String) = info match {
      case info: QualifiedInfo =>
        (underlying, info.name, info.kind.asInstanceOf[QualifiedNameKind].separator)
      case _ =>
        val (prefix, suffix, separator) = underlying.split
        (prefix, suffix.derived(info), separator)
    }

    override def isEmpty: Boolean = false
    override def encode: ThisName = underlying.encode.derived(info.map(NameTransformer.encode)) // encodes <init>
    override def decode: ThisName = underlying.decode.derived(info.map(_.decode))
    override def firstPart: SimpleName = underlying.firstPart
    override def lastPart: SimpleName = info match {
      case qual: QualifiedInfo => qual.name
      case _ => underlying.lastPart
    }
    protected def computeToString: String = info.mkString(underlying)
    override def debugString: String = s"${underlying.debugString}[$info]"
  }

  // ------ Name table ------------------------------------------------------------------

  /** Interning is split across `numStripes` independently-locked weak partitions (a power of two),
   *  so concurrent interning of differently-hashed names doesn't serialize on one lock (issue #1584). */
  private inline val stripeBits = 4
  private inline val numStripes = 1 << stripeBits

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

  /** Interns simple names weakly: maps a character sequence to the unique `SimpleName` holding it,
   *  reclaimed (with its characters) once unreferenced (issue #1584). Split into per-stripe
   *  `WeakHashSet`s with their own locks, since a single global lock degrades under the concurrent
   *  interning the process-global table sees (e.g. from multiple presentation compilers).
   *
   *  Names and characters are GC'd as soon as they're unreferenced; the light `Entry` wrappers are
   *  only unlinked when a stripe is next interned into. `compactStaleEntries()` drains them eagerly.
   */
  private class NameTable:
    import util.{Stats, WeakHashSet}
    import WeakHashSet.Entry
    import scala.annotation.tailrec

    /** Does the whole of `a` equal cs[offset..offset+len-1]? */
    private def equalsChars(a: Array[Char], cs: Array[Char], offset: Int, len: Int): Boolean =
      java.util.Arrays.equals(a, 0, a.length, cs, offset, offset + len)

    /** One weakly-interned partition of the table, with its own lock. */
    private final class Stripe extends WeakHashSet[SimpleName](initialCapacity = 0x10000 >> stripeBits):
      override def hash(x: SimpleName): Int = hashValue(x.chars, 0, x.length)
      override def isEqual(x: SimpleName, y: SimpleName): Boolean =
        equalsChars(x.chars, y.chars, 0, y.length)

      /** Find or create the unique name for cs[offset..offset+len-1], whose hash is `h`. */
      def enterIfNew(cs: Array[Char], offset: Int, len: Int, h: Int): SimpleName = synchronized {
        Stats.record(statsItem("put"))
        removeStaleEntries()
        val bucket = index(h)
        val oldHead = table(bucket)

        @tailrec
        def linkedListLoop(entry: Entry[SimpleName] | Null): SimpleName = entry match
          case null =>
            // Each name owns exactly its characters, so a GC'd name reclaims its chars too.
            val name = SimpleName(java.util.Arrays.copyOfRange(cs, offset, offset + len))
            addEntryAt(bucket, name, h, oldHead)
          case _ =>
            val e = entry.get
            if e != null && equalsChars(e.chars, cs, offset, len) then e
            else linkedListLoop(entry.tail)

        linkedListLoop(oldHead)
      }

      /** Drain GC'd entries from this stripe's bucket array (releases their `Entry` wrappers). */
      def compact(): Unit = synchronized { removeStaleEntries() }

    private val stripes: Array[Stripe] = Array.fill(numStripes)(new Stripe)

    /** Avalanche `h` before picking a stripe — the raw `hashValue`'s high bits are ~0 for short
     *  identifiers, which would pile them into stripe 0 (MurmurHash3 finalizer, as `util.HashSet`). */
    private def mix(h: Int): Int =
      val i = (h ^ (h >>> 16)) * 0x85EBCA6B
      i ^ (i >>> 13)

    // Only stripe selection needs the mixed hash; bucketing within a stripe uses raw `h` (= Stripe.hash).
    private def stripeOf(h: Int): Stripe = stripes(mix(h) & (numStripes - 1))

    def enterIfNew(cs: Array[Char], offset: Int, len: Int): SimpleName =
      val h = hashValue(cs, offset, len)
      stripeOf(h).enterIfNew(cs, offset, len, h)

    /** The total number of live interned names across all stripes. */
    def size: Int =
      var s = 0
      var i = 0
      while i < numStripes do { s += stripes(i).size; i += 1 }
      s

    /** Unlink the `Entry` wrappers of GC'd names across all stripes (otherwise done lazily on the
     *  next intern into each). The names/characters are already freed; safe to call at any time. */
    def compactStaleEntries(): Unit =
      var i = 0
      while i < numStripes do { stripes(i).compact(); i += 1 }

    /** The empty term name (interned like any other name, in its hash's stripe). */
    val empty: SimpleName = enterIfNew(Array.empty[Char], 0, 0)

    /** Create a term name from the characters in cs[offset..offset+len-1]. */
    def termName(cs: Array[Char], offset: Int, len: Int): SimpleName =
      enterIfNew(cs, offset, len)

    /** Create a type name from the characters in cs[offset..offset+len-1]. */
    def typeName(cs: Array[Char], offset: Int, len: Int): TypeName =
      termName(cs, offset, len).toTypeName

    /** Create a term name from the UTF8 encoded bytes in bs[offset..offset+len-1]. */
    def termName(bs: Array[Byte], offset: Int, len: Int): SimpleName =
      val chars = Codec.fromUTF8(bs, offset, len)
      termName(chars, 0, chars.length)

    /** Create a type name from the UTF8 encoded bytes in bs[offset..offset+len-1]. */
    def typeName(bs: Array[Byte], offset: Int, len: Int): TypeName =
      termName(bs, offset, len).toTypeName

    /** Create a term name from a string. */
    def termName(s: String): SimpleName = termName(s.toCharArray.nn, 0, s.length)

    /** Create a type name from a string. */
    def typeName(s: String): TypeName = typeName(s.toCharArray.nn, 0, s.length)
  end NameTable

  /** The process-global name table. Names are interned here weakly, so they (and their
   *  characters) are reclaimed once no tree/symbol references them (issue #1584).
   */
  @sharable
  private val nameTable = NameTable()

  /** Optional maintenance for long-lived hosts (e.g. a presentation compiler dropping a project):
   *  eagerly release the bookkeeping wrappers of GC'd names for a predictable footprint. The names
   *  themselves are reclaimed regardless. */
  private[dotty] def compactStaleEntries(): Unit = nameTable.compactStaleEntries()

  /** The term name represented by the empty string */
  val EmptyTermName: SimpleName = nameTable.empty

  /** Create a term name from the characters in cs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def termName(cs: Array[Char], offset: Int, len: Int): SimpleName =
    nameTable.termName(cs, offset, len)

  /** Create a type name from the characters in cs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def typeName(cs: Array[Char], offset: Int, len: Int): TypeName =
    nameTable.typeName(cs, offset, len)

  /** Create a term name from the UTF8 encoded bytes in bs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def termName(bs: Array[Byte], offset: Int, len: Int): SimpleName =
    nameTable.termName(bs, offset, len)

  /** Create a type name from the UTF8 encoded bytes in bs[offset..offset+len-1].
   *  Assume they are already encoded.
   */
  def typeName(bs: Array[Byte], offset: Int, len: Int): TypeName =
    nameTable.typeName(bs, offset, len)

  /** Create a term name from a string.
   *  See `sliceToTermName` in `Decorators` for a more efficient version
   *  which however requires a Context for its operation.
   */
  def termName(s: String): SimpleName = nameTable.termName(s)

  /** Create a type name from a string */
  def typeName(s: String): TypeName = nameTable.typeName(s)

  /** The type name represented by the empty string */
  val EmptyTypeName: TypeName = EmptyTermName.toTypeName

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
    private def compareSimpleNames(x: SimpleName, y: SimpleName): Int = {
      val until = x.length min y.length
      var i = 0
      while (i < until && x(i) == y(i)) i = i + 1
      if (i < until)
        if (x(i) < y(i)) -1
        else /*(x(i) > y(i))*/ 1
      else
        x.length - y.length
    }
    private def compareTermNames(x: TermName, y: TermName): Int = x match {
      case x: SimpleName =>
        y match {
          case y: SimpleName => compareSimpleNames(x, y)
          case _ => -1
        }
      case DerivedName(xPre, xInfo) =>
        y match {
          case DerivedName(yPre, yInfo) =>
            val s = compareInfos(xInfo, yInfo)
            if (s == 0) compareTermNames(xPre, yPre) else s
          case _ => 1
        }
    }
    def compare(x: Name, y: Name): Int =
      if (x.isTermName && y.isTypeName) 1
      else if (x.isTypeName && y.isTermName) -1
      else if (x eq y) 0
      else compareTermNames(x.toTermName, y.toTermName)
  }
}
