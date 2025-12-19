package dotty.tools

import dotty.tools.dotc.core.Caches.{FilteringCache, SynchronizedMapCache}
import dotty.tools.dotc.core.CacheStores.{CacheStore, DefaultCacheStore}
import dotty.tools.io.AbstractFile

object TestCacheStore extends CacheStore:
  /** Use the default global classpath cache. */
  val classPaths = DefaultCacheStore.classPaths

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
