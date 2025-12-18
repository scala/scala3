package dotty.tools.dotc

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.LongAdder

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.io.{AbstractFile, FileExtension}

trait GlobalCache:
  /** Get the content of a file, possibly caching it globally.
   *
   *  Implementations must be thread-safe.
   */
  def getFileContent(file: AbstractFile): Array[Byte]

object GlobalCache:
  /** A global cache that keeps file contents in memory without any size limit.
   *
   *  @param shouldCache
   *    A predicate that determines whether an [[AbstracFile]] should be cached.
   */
  class ConcurrentGlobalCache(shouldCache: AbstractFile => Boolean) extends GlobalCache:
    private val cache = ConcurrentHashMap[AbstractFile, Array[Byte]]()
    private val totalByExt = ConcurrentHashMap[FileExtension, LongAdder]()
    private val missesByExt = ConcurrentHashMap[FileExtension, LongAdder]()
    private val uncachedByExt = ConcurrentHashMap[FileExtension, LongAdder]()

    override def getFileContent(file: AbstractFile): Array[Byte] =
      totalByExt.computeIfAbsent(file.ext,  _ => LongAdder()).increment()
      if shouldCache(file) then
        cache.computeIfAbsent(file, f =>
          missesByExt.computeIfAbsent(file.ext,  _ => LongAdder()).increment()
          //println(s"Caching file: ${file.canonicalPath}")
          f.toByteArray
        )
      else
        uncachedByExt.computeIfAbsent(file.ext,  _ => LongAdder()).increment()
        file.toByteArray

    final def printCacheStats(): Unit =
      println(this.getClass.getSimpleName + " statistics:")
      totalByExt.forEach: (ext, totalAdder) =>
        val misses = missesByExt.computeIfAbsent(ext, _ => LongAdder()).longValue()
        val uncached = uncachedByExt.computeIfAbsent(ext, _ => LongAdder()).longValue()
        val total = totalAdder.longValue()
        val hits = total - misses - uncached
        val files = cache.asScala.filter(_._1.ext == ext)
        val sizeMB = files.map(_._2.length.toLong).sum.toDouble / (1024 * 1024)
        println(f"- *.$ext: hits: $hits, misses: $misses, uncached: $uncached, total: $total, cache size: $sizeMB%.2f MB")

  /** A global cache that does not cache anything.
   *
   *  This is the default value for [[GlobalCache]].
   */
  object NoGlobalCache extends GlobalCache:
    override def getFileContent(file: AbstractFile): Array[Byte] =
      file.toByteArray
