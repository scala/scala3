package dotty.tools.dotc

import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.io.{AbstractFile, ClassPath}

/** Global cache that can be shared across [[Driver]] instances.
 *
 *  This class is thread-safe.
 */
final class GlobalCache():
  import GlobalCache.FileBasedCache

  private val classPathCache = FileBasedCache[ClassPath]()

  def getOrCreateClassPath(key: AbstractFile, createValue: => ClassPath)(using Context): ClassPath =
    classPathCache.getOrCreate(key.file.nn.toPath, () => createValue)

object GlobalCache:

  /** A cache for values associated with files on disk, that invalidates
   *  the cached value when the file is modified.
   *
   *  See https://github.com/scala/bug/issues/10295 for some context on the
   *  invalidation strategy.
   *
   *  Moved from [[ZipAndJarFileLookupFactory]] in December 2025.
   *
   *  @author @allanrenucci
   */
  private class FileBasedCache[T]:
    private case class Stamp(lastModified: FileTime, fileKey: Object)
    private val cache = collection.mutable.Map.empty[java.nio.file.Path, (Stamp, T)]

    def getOrCreate(path: java.nio.file.Path, create: () => T): T =
      cache.synchronized:
        val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
        val lastModified = attrs.lastModifiedTime()
        // null on some platforms, but that's okay, we just use the last
        // modified timestamp as our stamp in that case
        val fileKey = attrs.fileKey()
        val stamp = Stamp(lastModified, fileKey)
        cache.get(path) match
          case Some((cachedStamp, cached)) if cachedStamp == stamp =>
            cached
          case _ =>
            val value = create()
            cache.put(path, (stamp, value))
            value
