package dotty.tools.dotc
package core

import Types.*, Contexts.*, util.Stats.*, Hashable.*, Names.*
import config.Config
import Symbols.Symbol
import Decorators.*
import util.{WeakHashSet, Stats}
import WeakHashSet.{Entry as WeakEntry}
import scala.annotation.tailrec
import scala.util.hashing.{MurmurHash3 => hashing}

class Uniques extends WeakHashSet[Type](Config.initialUniquesCapacity):
  override def hash(x: Type): Int = x.hash
  override def isEqual(x: Type, y: Type) = x.eql(y)

/** Defines operation `unique` for hash-consing types.
 *  Also defines specialized hash sets for hash consing uniques of a specific type.
 *  All sets offer a `enterIfNew` method which checks whether a type
 *  with the given parts exists already and creates a new one if not.
 */
object Uniques:

  private inline def recordCaching(tp: Type): Unit = recordCaching(tp.hash, tp.getClass)
  private inline def recordCaching(h: Int, clazz: Class[?]): Unit =
    if monitored then
      if h == NotCached then
        record("uncached-types")
        record(s"uncached: $clazz")
      else
        record("cached-types")
        record(s"cached: $clazz")

  def unique[T <: Type](tp: T)(using Context): T =
    recordCaching(tp)
    if tp.hash == NotCached then tp
    else ctx.uniques.put(tp).asInstanceOf[T]

  final class NamedTypeUniques extends WeakHashSet[NamedType](Config.initialUniquesCapacity * 4) with Hashable:
    override def hash(x: NamedType): Int = x.hash

    def enterIfNew(prefix: Type, designator: Designator, isTerm: Boolean)(using Context): NamedType =
      val h = doHash(null, designator, prefix)
      if monitored then recordCaching(h, classOf[NamedType])
      def newType =
        try
          if isTerm then new CachedTermRef(prefix, designator, h)
          else new CachedTypeRef(prefix, designator, h)
        catch case ex: InvalidPrefix => badPrefix(prefix, designator)
      if h == NotCached then newType
      else
        // Inlined from WeakHashSet#put
        Stats.record(statsItem("put"))
        removeStaleEntries()
        val bucket = index(h)
        val oldHead = table(bucket)

        @tailrec
        def linkedListLoop(entry: WeakEntry[NamedType] | Null): NamedType = entry match
          case null                    => addEntryAt(bucket, newType, h, oldHead)
          case _                       =>
            if entry.hash == h then
              val e = entry.get
              if e != null && (e.prefix eq prefix) && (e.designator eq designator) && (e.isTerm == isTerm) then e
              else linkedListLoop(entry.tail)
            else linkedListLoop(entry.tail)

        linkedListLoop(oldHead)
      end if
    end enterIfNew

    private def badPrefix(prefix: Type, desig: Designator)(using Context): Nothing =
      def name = desig match
        case desig: Name => desig
        case desig: Symbol => desig.name
      throw TypeError(em"invalid prefix $prefix when trying to form $prefix . $name")

  end NamedTypeUniques

  /** Weak paged hash-consing table for AppliedTypes.
   *  AppliedTypes can be produced in large volumes inside one compiler run, so
   *  stale entries must be shed before the next ContextBase.reset() boundary.
   *  Occupancy bits are an over-approximation: stale-entry removals may leave a
   *  bit set, but that only misses an empty-split shortcut and never hides an
   *  occupied bucket.
   */
  final class AppliedUniques extends WeakHashSet[AppliedType](1) with Hashable:
    private type AppliedEntry = WeakEntry[AppliedType]
    private type BucketPage = Array[AppliedEntry | Null]
    private type OccupancyPage = Array[Long]

    private inline val PageBits = 10
    private inline val PageSize = 1 << PageBits
    private inline val PageMask = PageSize - 1
    private inline val OccupancyWordBits = 6
    private inline val OccupancyWordSize = 1 << OccupancyWordBits
    private inline val OccupancyWordMask = OccupancyWordSize - 1
    private inline val OccupancyWords = PageSize >>> OccupancyWordBits
    private inline val InitialBuckets = Config.initialUniquesCapacity * 2

    private val appliedHashSeed = hashSeed
    private var pages = new Array[BucketPage | Null](InitialBuckets >>> PageBits)
    private var occupancyPages = new Array[OccupancyPage | Null](InitialBuckets >>> PageBits)
    private var bucketLevel = InitialBuckets
    private var splitBucket = 0
    private var activeBuckets = InitialBuckets
    private var pagedThreshold = computePagedThreshold()

    override def hash(x: AppliedType): Int = x.hash

    private def computePagedThreshold(): Int = (activeBuckets + 3) >>> 2

    private def bucketIndex(h: Int): Int =
      val base = h & (bucketLevel - 1)
      if base < splitBucket then h & ((bucketLevel << 1) - 1) else base

    private def ensurePage(bucket: Int): BucketPage =
      val pageIndex = bucket >>> PageBits
      if pageIndex >= pages.length then
        val newLength = math.max(pageIndex + 1, pages.length << 1)
        val newPages = new Array[BucketPage | Null](newLength)
        Array.copy(pages, 0, newPages, 0, pages.length)
        pages = newPages
        val newOccupancyPages = new Array[OccupancyPage | Null](newLength)
        Array.copy(occupancyPages, 0, newOccupancyPages, 0, occupancyPages.length)
        occupancyPages = newOccupancyPages
      var page = pages(pageIndex)
      if page == null then
        page = new Array[AppliedEntry | Null](PageSize)
        pages(pageIndex) = page
        occupancyPages(pageIndex) = new Array[Long](OccupancyWords)
      page

    private def markBucketOccupied(bucket: Int): Unit =
      val wordIndex = (bucket & PageMask) >>> OccupancyWordBits
      val page = occupancyPages(bucket >>> PageBits).nn
      page(wordIndex) = page(wordIndex) | (1L << (bucket & OccupancyWordMask))

    private def markBucketEmpty(bucket: Int): Unit =
      val pageIndex = bucket >>> PageBits
      if pageIndex < occupancyPages.length then
        val page = occupancyPages(pageIndex)
        if page != null then
          val wordIndex = (bucket & PageMask) >>> OccupancyWordBits
          page(wordIndex) = page(wordIndex) & ~(1L << (bucket & OccupancyWordMask))

    private def bucketHead(bucket: Int): AppliedEntry | Null =
      val pageIndex = bucket >>> PageBits
      if pageIndex >= pages.length then null
      else
        val page = pages(pageIndex)
        if page == null then null else page(bucket & PageMask)

    private def bucketHead_=(bucket: Int, head: AppliedEntry | Null): Unit =
      ensurePage(bucket)(bucket & PageMask) = head

    private def removePaged(bucket: Int, prevEntry: AppliedEntry | Null, entry: AppliedEntry): Unit =
      Stats.record(statsItem("remove"))
      prevEntry match
        case null =>
          bucketHead_=(bucket, entry.tail)
          if entry.tail == null then markBucketEmpty(bucket)
        case _ => prevEntry.tail = entry.tail
      count -= 1

    override protected def removeStaleEntries(): Unit =
      def poll(): AppliedEntry | Null = queue.poll().asInstanceOf

      @tailrec
      def queueLoop(): Unit =
        val stale = poll()
        if stale != null then
          val bucket = bucketIndex(stale.hash)

          @tailrec
          def linkedListLoop(prevEntry: AppliedEntry | Null, entry: AppliedEntry | Null): Unit =
            if entry != null then
              if stale eq entry then removePaged(bucket, prevEntry, entry)
              else linkedListLoop(entry, entry.tail)

          linkedListLoop(null, bucketHead(bucket))
          queueLoop()

      queueLoop()

    private def splitOneBucket(): Unit =
      Stats.record(statsItem("resize"))
      val oldBucket = splitBucket
      val newBucket = activeBuckets
      val oldHead = bucketHead(oldBucket)
      activeBuckets += 1

      if oldHead != null then
        val expandedMask = (bucketLevel << 1) - 1
        var oldPart: AppliedEntry | Null = null
        var newPart: AppliedEntry | Null = null
        var entry: AppliedEntry | Null = oldHead
        while entry != null do
          val next = entry.tail
          if (entry.hash & expandedMask) == newBucket then
            entry.tail = newPart
            newPart = entry
          else
            entry.tail = oldPart
            oldPart = entry
          entry = next
        bucketHead_=(oldBucket, oldPart)
        if oldPart == null then markBucketEmpty(oldBucket)
        if newPart != null then
          bucketHead_=(newBucket, newPart)
          markBucketOccupied(newBucket)

      splitBucket += 1
      if splitBucket == bucketLevel then
        splitBucket = 0
        bucketLevel <<= 1
      pagedThreshold = computePagedThreshold()

    private def emptySplitRun(maxSkips: Int): Int =
      val firstBucket = splitBucket
      val endBucket = math.min(bucketLevel, firstBucket + maxSkips)
      var bucket = firstBucket
      while bucket < endBucket do
        val pageIndex = bucket >>> PageBits
        val pageLimit = math.min(endBucket, (pageIndex + 1) << PageBits)
        val page =
          if pageIndex < occupancyPages.length then occupancyPages(pageIndex)
          else null
        if page == null then bucket = pageLimit
        else
          var wordBucket = bucket
          while wordBucket < pageLimit do
            val wordIndex = (wordBucket & PageMask) >>> OccupancyWordBits
            val wordLimit = math.min(pageLimit, (wordBucket + OccupancyWordSize) & ~OccupancyWordMask)
            val fromMask = -1L << (wordBucket & OccupancyWordMask)
            val toBit = wordLimit & OccupancyWordMask
            val toMask = if toBit == 0 then -1L else (1L << toBit) - 1
            val occupied = page(wordIndex) & fromMask & toMask
            if occupied != 0 then
              return ((wordBucket & ~OccupancyWordMask) + java.lang.Long.numberOfTrailingZeros(occupied)) - firstBucket
            wordBucket = wordLimit
          bucket = pageLimit
      endBucket - firstBucket

    private def skipEmptySplitBuckets(): Boolean =
      val targetActiveBuckets = (count << 2) - 3
      val maxSkips = targetActiveBuckets - activeBuckets
      if maxSkips <= 0 then false
      else
        val skipped = emptySplitRun(maxSkips)
        if skipped == 0 then false
        else
          activeBuckets += skipped
          splitBucket += skipped
          if splitBucket == bucketLevel then
            splitBucket = 0
            bucketLevel <<= 1
          pagedThreshold = computePagedThreshold()
          true

    private def addPagedEntryAt(bucket: Int, elem: AppliedType, elemHash: Int, oldHead: AppliedEntry | Null): AppliedType =
      Stats.record(statsItem("addEntryAt"))
      bucketHead_=(bucket, new WeakEntry(elem, elemHash, oldHead, queue))
      if oldHead == null then markBucketOccupied(bucket)
      count += 1
      while count > pagedThreshold do
        if !skipEmptySplitBuckets() then splitOneBucket()
      elem

    override def lookup(elem: AppliedType): AppliedType | Null = (elem: AppliedType | Null) match
      case null => throw new NullPointerException("WeakHashSet cannot hold nulls")
      case _ =>
        Stats.record(statsItem("lookup"))
        removeStaleEntries()
        val h = hash(elem)
        val bucket = bucketIndex(h)

        @tailrec
        def linkedListLoop(entry: AppliedEntry | Null): AppliedType | Null = entry match
          case null => null
          case _ =>
            if entry.hash == h then
              val entryElem = entry.get
              if entryElem != null && isEqual(elem, entryElem) then entryElem
              else linkedListLoop(entry.tail)
            else linkedListLoop(entry.tail)

        linkedListLoop(bucketHead(bucket))

    override def put(elem: AppliedType): AppliedType = (elem: AppliedType | Null) match
      case null => throw new NullPointerException("WeakHashSet cannot hold nulls")
      case _ =>
        Stats.record(statsItem("put"))
        removeStaleEntries()
        val h = hash(elem)
        val bucket = bucketIndex(h)
        val oldHead = bucketHead(bucket)

        @tailrec
        def linkedListLoop(entry: AppliedEntry | Null): AppliedType = entry match
          case null => addPagedEntryAt(bucket, elem, h, oldHead)
          case _ =>
            if entry.hash == h then
              val entryElem = entry.get
              if entryElem != null && isEqual(elem, entryElem) then entryElem
              else linkedListLoop(entry.tail)
            else linkedListLoop(entry.tail)

        linkedListLoop(oldHead)

    override def -=(elem: AppliedType): Unit = (elem: AppliedType | Null) match
      case null =>
      case _ =>
        Stats.record(statsItem("-="))
        removeStaleEntries()
        val h = hash(elem)
        val bucket = bucketIndex(h)

        @tailrec
        def linkedListLoop(prevEntry: AppliedEntry | Null, entry: AppliedEntry | Null): Unit =
          if entry != null then
            if entry.hash == h then
              val entryElem = entry.get
              if entryElem != null && isEqual(elem, entryElem) then removePaged(bucket, prevEntry, entry)
              else linkedListLoop(entry, entry.tail)
            else linkedListLoop(entry, entry.tail)

        linkedListLoop(null, bucketHead(bucket))

    override def clear(resetToInitial: Boolean): Unit =
      @tailrec def drainQueue(): Unit = if queue.poll() != null then drainQueue()

      if count != 0 then
        if resetToInitial then
          pages = new Array[BucketPage | Null](InitialBuckets >>> PageBits)
          occupancyPages = new Array[OccupancyPage | Null](InitialBuckets >>> PageBits)
          bucketLevel = InitialBuckets
          splitBucket = 0
          activeBuckets = InitialBuckets
          pagedThreshold = computePagedThreshold()
        else
          pages = new Array[BucketPage | Null](pages.length)
          occupancyPages = new Array[OccupancyPage | Null](occupancyPages.length)
        count = 0
      drainQueue()

    override def size: Int =
      removeStaleEntries()
      count

    override def iterator: Iterator[AppliedType] =
      removeStaleEntries()

      new collection.AbstractIterator[AppliedType]:
        private var currentBucket = activeBuckets
        private var entry: AppliedEntry | Null = null
        private var lookaheadElement: AppliedType | Null = null

        @tailrec
        def hasNext: Boolean =
          while entry == null && currentBucket > 0 do
            currentBucket -= 1
            entry = bucketHead(currentBucket)

          val e = entry
          if e == null then false
          else
            lookaheadElement = e.get
            if lookaheadElement == null then
              entry = e.tail
              hasNext
            else true

        def next(): AppliedType =
          if lookaheadElement == null then
            throw new IndexOutOfBoundsException("next on an empty iterator")
          else
            val result = lookaheadElement.nn
            lookaheadElement = null
            entry = entry.nn.tail
            result

    private inline def sameArgs1(args: List[Type], arg: Type): Boolean =
      !args.isEmpty && (args.head eq arg) && args.tail.isEmpty

    private inline def sameArgs2(args: List[Type], arg1: Type, arg2: Type): Boolean =
      if args.isEmpty || (args.head ne arg1) then false
      else
        val rest = args.tail
        !rest.isEmpty && (rest.head eq arg2) && rest.tail.isEmpty

    def enterIfNew(tycon: Type, arg: Type): AppliedType =
      val argsEqHash = 31 + System.identityHashCode(arg)
      val tyconHash = tycon.hash
      val h =
        if tyconHash == NotCached then NotCached
        else
          val argHash = arg.hash
          if argHash == NotCached then NotCached
          else finishHash(hashing.mix(hashing.mix(appliedHashSeed, tyconHash), argHash), 2)
      def newType(argsEqHash: Int) = new CachedAppliedType(tycon, arg :: Nil, h, argsEqHash)
      if monitored then recordCaching(h, classOf[CachedAppliedType])
      if h == NotCached then newType(argsEqHash)
      else
        // Inlined from WeakHashSet#put
        Stats.record(statsItem("put"))
        removeStaleEntries()
        val bucket = bucketIndex(h)
        val oldHead = bucketHead(bucket)
        val candidateArgsEqHash = argsEqHash

        @tailrec
        def linkedListLoop(entry: AppliedEntry | Null): AppliedType = entry match
          case null                    => addPagedEntryAt(bucket, newType(candidateArgsEqHash), h, oldHead)
          case _                       =>
            if entry.hash == h then
              val e = entry.get
              if e != null && (e.tycon eq tycon) && e.argsEqHash == candidateArgsEqHash
                 && sameArgs1(e.args, arg) then e
              else linkedListLoop(entry.tail)
            else linkedListLoop(entry.tail)

        linkedListLoop(oldHead)
      end if

    def enterIfNew(tycon: Type, arg1: Type, arg2: Type): AppliedType =
      val argsEqHash = (31 + System.identityHashCode(arg1)) * 31 + System.identityHashCode(arg2)
      val tyconHash = tycon.hash
      val h =
        if tyconHash == NotCached then NotCached
        else
          val arg1Hash = arg1.hash
          if arg1Hash == NotCached then NotCached
          else
            val arg2Hash = arg2.hash
            if arg2Hash == NotCached then NotCached
            else
              finishHash(hashing.mix(hashing.mix(hashing.mix(appliedHashSeed, tyconHash), arg1Hash), arg2Hash), 3)
      def newType(argsEqHash: Int) = new CachedAppliedType(tycon, arg1 :: arg2 :: Nil, h, argsEqHash)
      if monitored then recordCaching(h, classOf[CachedAppliedType])
      if h == NotCached then newType(argsEqHash)
      else
        // Inlined from WeakHashSet#put
        Stats.record(statsItem("put"))
        removeStaleEntries()
        val bucket = bucketIndex(h)
        val oldHead = bucketHead(bucket)
        val candidateArgsEqHash = argsEqHash

        @tailrec
        def linkedListLoop(entry: AppliedEntry | Null): AppliedType = entry match
          case null                    => addPagedEntryAt(bucket, newType(candidateArgsEqHash), h, oldHead)
          case _                       =>
            if entry.hash == h then
              val e = entry.get
              if e != null && (e.tycon eq tycon) && e.argsEqHash == candidateArgsEqHash
                 && sameArgs2(e.args, arg1, arg2) then e
              else linkedListLoop(entry.tail)
            else linkedListLoop(entry.tail)

        linkedListLoop(oldHead)
      end if

    def enterIfNew(tycon: Type, args: List[Type]): AppliedType =
      var argsEqHash = 1
      val tyconHash = tycon.hash
      var hashValid = tyconHash != NotCached
      var hash = if hashValid then hashing.mix(appliedHashSeed, tyconHash) else NotCached
      var len = 1
      var xs = args
      while !xs.isEmpty do
        val arg = xs.head
        argsEqHash = argsEqHash * 31 + System.identityHashCode(arg)
        if hashValid then
          val argHash = arg.hash
          if argHash == NotCached then hashValid = false
          else
            hash = hashing.mix(hash, argHash)
            len += 1
        xs = xs.tail
      val h = if hashValid then finishHash(hash, len) else NotCached
      def newType(argsEqHash: Int) = new CachedAppliedType(tycon, args, h, argsEqHash)
      if monitored then recordCaching(h, classOf[CachedAppliedType])
      if h == NotCached then newType(argsEqHash)
      else
        // Inlined from WeakHashSet#put
        Stats.record(statsItem("put"))
        removeStaleEntries()
        val bucket = bucketIndex(h)
        val oldHead = bucketHead(bucket)
        // Pre-filter probe: compare a cheap identity-hash of args
        // before falling into the eqElements list walk.
        val candidateArgsEqHash = argsEqHash

        @tailrec
        def linkedListLoop(entry: AppliedEntry | Null): AppliedType = entry match
          case null                    => addPagedEntryAt(bucket, newType(candidateArgsEqHash), h, oldHead)
          case _                       =>
            if entry.hash == h then
              val e = entry.get
              if e != null && (e.tycon eq tycon) && e.argsEqHash == candidateArgsEqHash
                 && ((e.args eq args) || e.args.eqElements(args)) then e
              else linkedListLoop(entry.tail)
            else linkedListLoop(entry.tail)

        linkedListLoop(oldHead)
      end if
  end AppliedUniques
end Uniques
