package dotty.tools.dotc.core

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.LongAdder

import scala.collection.mutable.Map

import dotty.tools.io.AbstractFile

object Caches:

  /** Cache for values of type `V`, associated with keys of type `K`. */
  trait Cache[K, V]:

    /** Get the value associated with `key` from the cache, or compute it using
     *  the by-name parameter `value`.
     *
     *  The value is cached iff `mightContain(key) == true`.
     */
    def apply(key: K, value: => V): V

    /** Check whether the cache might contain a value for `key`.
     *
     *  `true` means that the cache will cache the value for `key` if requested
     *  via [[apply]], not that it has already cached it.
     */
    def mightContain(key: K): Boolean

    def stats(): CacheStats

    override def toString: String =
      s"${this.getClass.getSimpleName}(stats() = ${stats()})"

  /** Statistics about a cache */
  final case class CacheStats(total: Long, misses: Long, uncached: Long):
    val hits: Long = total - misses - uncached

    override def toString: String =
      s"(total = $total, hits = $hits, misses = $misses, uncached = $uncached)"

  /** A no-op cache implementation that does not cache anything. */
  final class NoopCache[K, V] extends Cache[K, V]:
    private var total = 0L

    def apply(key: K, value: => V): V =
      total += 1
      value

    def mightContain(key: K): Boolean =
      false

    def stats(): CacheStats =
      CacheStats(total, misses = 0, uncached = total)

  /** Default value for stamp function that indicates no stamping. */
  private def noStamp[K](key: K): Option[Unit] = Some(())

  /** A thread-unsafe cache implementation based on a mutable [[Map]].
   *
   *  Entries are not evicted.
   *
   *  @param getStamp
   *    Function to obtain a stamp for a given key. If the function returns
   *    `None`, no caching is performed for that key. If the function returns
   *    `Some(stamp)`, the stamp is used to validate cached entries: cache
   *    values are only reused if the stamp matches the cached stamp.
   */
  final class MapCache[K, S, V](getStamp: K => Option[S] = noStamp) extends Cache[K, V]:
    private val map = Map.empty[K, (S, V)]
    private var total = 0L
    private var misses = 0L
    private var uncached = 0L

    def apply(key: K, value: => V): V =
      total += 1
      getStamp(key) match
        case None =>
          uncached += 1
          value
        case Some(stamp) =>
          map.get(key) match
            case Some((cachedStamp, cachedValue)) if cachedStamp == stamp =>
              cachedValue
            case _ =>
              misses += 1
              val v = value
              map.put(key, (stamp, v))
              v

    def mightContain(key: K): Boolean =
      getStamp(key).isDefined

    def stats(): CacheStats =
      CacheStats(total, misses, uncached)

  /** A thread-safe cache implementation based on a Java [[ConcurrentHashMap]].
   *
   *  Entries are not evicted.
   */
  final class SynchronizedMapCache[K, S, V](getStamp: K => Option[S] = noStamp) extends Cache[K, V]:
    private val map = ConcurrentHashMap[K, (S, V)]()
    private val total = LongAdder()
    private val misses = LongAdder()
    private val uncached = LongAdder()

    def apply(key: K, value: => V): V =
      total.increment()
      getStamp(key) match
        case None =>
          uncached.increment()
          value
        case Some(stamp) =>
          map.compute(
            key,
            (_, cached) =>
              if cached != null && cached._1 == stamp then
                cached
              else
                misses.increment()
                (stamp, value)
          )._2

    def mightContain(key: K): Boolean =
      getStamp(key).isDefined

    def stats(): CacheStats =
      CacheStats(total.longValue(), misses.longValue(), uncached.longValue())

  /** A cache where keys are [[AbstractFile]]s.
   *
   *  The cache uses file modification time and file key (inode) as stamp to
   *  invalidate cached entries when the underlying file has changed.
   *
   *  For files with an underlying source (e.g. files inside a zip/jar), the
   *  stamp is obtained from the underlying source file.
   *
   *  If the [[AbstractFile]] does not correspond to a physical file on disk, no
   *  caching is performed.
   *
   *  See https://github.com/scala/bug/issues/10295 for discussion about the
   *  invalidation strategy.
   */
  final class FileBasedCache[V]() extends Cache[AbstractFile, V]:
    private case class FileStamp(lastModified: FileTime, fileKey: Object)

    private def getPath(abstractFile: AbstractFile): Option[Path] =
      abstractFile.underlyingSource match
        case Some(underlyingSource) if underlyingSource ne abstractFile =>
          getPath(underlyingSource)
        case _ =>
          val javaPath = abstractFile.jpath
          if javaPath != null then Some(javaPath) else None

    private def getFileStamp(abstractFile: AbstractFile): Option[FileStamp] =
      getPath(abstractFile) match
        case Some(path) =>
          val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
          val lastModified = attrs.lastModifiedTime()
          // This can be `null` on some platforms, but that's okay, we just use
          // the last modified timestamp as our stamp in that case.
          val fileKey = attrs.fileKey()
          Some(FileStamp(lastModified, fileKey))
        case None =>
          None

    private val underlying = SynchronizedMapCache[AbstractFile, FileStamp, V](getFileStamp)

    def apply(key: AbstractFile, value: => V): V =
      underlying(key, value)

    def mightContain(key: AbstractFile): Boolean =
      // We just check that a path exists here to avoi IO. `getFileStamp` will
      // return `None` iff `getPath` returns `None`.
      getPath(key).isDefined

    def stats(): CacheStats =
      underlying.stats()

    override def toString: String =
      s"FileBasedCache(${underlying.toString})"

  /** Filtering cache wrapper that only caches values whose key satisfies a
   *  given predicate.
   *
   *  @param underlying
   *    Underlying cache
   *  @param shouldCache
   *    Should the value associated with the given key should be cached?
   */
  final class FilteringCache[K, V](underlying: Cache[K, V], shouldCache: K => Boolean) extends Cache[K, V]:
    private val uncached = LongAdder()

    def apply(key: K, value: => V): V =
      if shouldCache(key) then
        underlying(key, value)
      else
        uncached.increment()
        value

    def stats(): CacheStats =
      val baseStats = underlying.stats()
      CacheStats(
        total = baseStats.total + uncached.longValue(),
        misses = baseStats.misses,
        uncached = baseStats.uncached + uncached.longValue()
      )

    def mightContain(key: K): Boolean =
      shouldCache(key) && underlying.mightContain(key)

    override def toString: String =
      s"FilteringCache(${underlying.toString}, uncached = ${uncached.longValue()})"
