package dotty.tools

import dotty.tools.dotc.core.Caches.{FilteringCache, SynchronizedMapCache}
import dotty.tools.dotc.core.CacheStores.{CacheStore, DefaultCacheStore}
import dotty.tools.io.AbstractFile

object TestCacheStore extends CacheStore:
  /** Use the default global classpath cache. */
  val classPaths = DefaultCacheStore.classPaths

  /** Standard library sources directory */
  private val stdLibDir = "library/src"

  /** Cache files across runs, without invalidation. */
  val files = FilteringCache(SynchronizedMapCache(), _.startsWith((stdLibDir)))

  /** Cache source files across runs, without invalidation.
   *
   *  We use a [[SynchronizedMapCache]] and not a [[FileBasedCache]] here
   *  because we assume that source files in `library/src` do not change during
   *  a test run.
   */
  val sources = FilteringCache(SynchronizedMapCache(), _.canonicalPath.startsWith(stdLibDir))

  /** Test output directory */
  private val outDir = "out"

  /** Cache class bytes across runs, except for classes in the `out` directory.
   *
   *  Classes in the `out` directory are generated during tests, so we do not
   *  want to cache them.
   */
  val classBytes = FilteringCache(SynchronizedMapCache(), !_.canonicalPath.startsWith(outDir))
