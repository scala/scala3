package dotty.tools.dotc.core

import dotty.tools.dotc.core.Caches.{Cache, FileBasedCache, NoopCache}
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{AbstractFile, ClassPath}

object CacheStores:

  /** A store of caches used by the compiler.
   *
   *  These caches can be shared across different runs.
   *
   *  Set on a [[Context]] via `setCacheStore` and retrieved via `cacheStore`.
   */
  trait CacheStore:
    def classPaths: Cache[AbstractFile, ClassPath]
    def files: Cache[TermName, AbstractFile]
    def sources: Cache[AbstractFile, SourceFile]
    def classBytes: Cache[AbstractFile, Array[Byte]]

    override def toString: String =
      s"""CacheStore(
         |  classPaths = $classPaths,
         |  files = $files,
         |  sources = $sources
         |  classBytes = $classBytes
         |)""".stripMargin

  /** Default, per-run cache store implementation. */
  object DefaultCacheStore extends CacheStore:

    /** A unique global cache for classpaths, shared across all runs.
     *
     *  This instance is thread-safe.
     */
    val classPaths = FileBasedCache()

    /** By default, we do not cache files across runs.
     *
     *  Regardless, files are always cached within a single run via
     *  `ContextBase.files`. See also `Context.getFile`.
     */
    val files = NoopCache()

    /** By default, we do not cache source files across runs.
     *
     *  Regardless, source files are always cached within a single run via
     *  `ContextBase.sources`. See also `Context.getSource`.
     */
    val sources = NoopCache()

    /** By default, we do not cache class bytes across runs. */
    val classBytes = NoopCache()

    /** By default, we do not cache tasty loaders. */
    val tastyLoaders = NoopCache()
