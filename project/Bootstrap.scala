package sbt

import sbt.internal.classpath.ClassLoaderCache
import sbt.internal.inc.ScalaInstance

// This class needs to be in package sbt to access the ClassLoaderCache
object Bootstrap {
  /** A ScalaInstance (without scaladoc). */
  def makeScalaInstance(
    state: State,
    version: String,
    libraryJars: Array[File],
    compilerJars: Array[File],
    topLoader: ClassLoader
  ): ScalaInstance = {
    // `extendedClassLoaderCache` is package private in package sbt
    val cache = state.extendedClassLoaderCache

    val libraryLoader = cache(libraryJars.toList, topLoader)
    val compilerLoader = cache(compilerJars.toList, libraryLoader)

    new ScalaInstance(
      version = version,
      loader = compilerLoader,
      loaderCompilerOnly = compilerLoader,
      loaderLibraryOnly = libraryLoader,
      libraryJars = libraryJars,
      compilerJars = compilerJars,
      allJars = libraryJars ++ compilerJars,
      explicitActual = Some(version)
    )
  }

  /** A ScalaInstance identical to `base` but with additional jars for scaladoc. */
  def makeDocScalaInstance(
    state: State,
    base: ScalaInstance,
    docJars: Array[File]
  ): ScalaInstance = {
    val cache = state.extendedClassLoaderCache

    val fullLoader = cache(docJars.toList, base.loaderCompilerOnly)

    new ScalaInstance(
      version = base.version,
      loader = fullLoader,
      loaderCompilerOnly = base.loaderCompilerOnly,
      loaderLibraryOnly = base.loaderLibraryOnly,
      libraryJars = base.libraryJars,
      compilerJars = base.compilerJars,
      allJars = base.allJars ++ docJars,
      explicitActual = base.explicitActual
    )
  }
}
