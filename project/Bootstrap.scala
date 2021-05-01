package sbt

import sbt.internal.classpath.ClassLoaderCache
import sbt.internal.inc.ScalaInstance

// This class needs to be in package sbt to access the ClassLoaderCache
object Bootstrap {
  def makeScalaInstance(
    state: State,
    version: String,
    libraryJars: Array[File],
    compilerJars: Array[File],
    docJars: Array[File],
    topLoader: ClassLoader
  ): ScalaInstance = {
    // `extendedClassLoaderCache` is package private in package sbt
    val cache = state.extendedClassLoaderCache

    val libraryLoader = cache(libraryJars.toList, topLoader)
    val compilerLoader = cache(compilerJars.toList, libraryLoader)
    val fullLoader = cache(docJars.toList, compilerLoader)

    new ScalaInstance(
      version = version,
      loader = fullLoader,
      loaderCompilerOnly = compilerLoader,
      loaderLibraryOnly = libraryLoader,
      libraryJars = libraryJars,
      compilerJars = compilerJars,
      allJars = libraryJars ++ compilerJars ++ docJars,
      explicitActual = Some(version)
    )
  }
}