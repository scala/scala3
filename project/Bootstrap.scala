package sbt

import sbt.internal.classpath.ClassLoaderCache
import sbt.internal.inc.ScalaInstance

object Bootstrap {
  // copied from https://github.com/sbt/sbt/blob/33e2bfe2810246c8e4e1b37ec110d15e588a4fce/main/src/main/scala/sbt/Defaults.scala#L1120-L1165
  def makeScalaInstance(
    version: String,
    libraryJars: Array[File],
    allCompilerJars: Seq[File],
    allDocJars: Seq[File],
    state: State,
    topLoader: ClassLoader
  ): ScalaInstance = {
    val classLoaderCache = state.extendedClassLoaderCache
    val compilerJars = allCompilerJars.filterNot(libraryJars.contains).distinct.toArray
    val docJars = allDocJars
      .filterNot(jar => libraryJars.contains(jar) || compilerJars.contains(jar))
      .distinct.toArray
    val allJars = libraryJars ++ compilerJars ++ docJars

    val libraryLoader = classLoaderCache(libraryJars.toList, topLoader)
    val compilerLoader = classLoaderCache(compilerJars.toList, libraryLoader)
    val fullLoader =
      if (docJars.isEmpty) compilerLoader
      else classLoaderCache(docJars.distinct.toList, compilerLoader)
    new ScalaInstance(
      version = version,
      loader = fullLoader,
      loaderCompilerOnly = compilerLoader,
      loaderLibraryOnly = libraryLoader,
      libraryJars = libraryJars,
      compilerJars = compilerJars,
      allJars = allJars,
      explicitActual = Some(version)
    )
  }
}