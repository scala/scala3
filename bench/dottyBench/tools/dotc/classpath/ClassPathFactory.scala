/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dottyBench.tools.dotc.classpath

import dottyBench.tools.io.{AbstractFile, VirtualDirectory}
import FileUtils.AbstractFileOps
import dottyBench.tools.io.ClassPath
import dottyBench.tools.dotc.core.Contexts._

/**
 * Provides factory methods for classpath. When creating classpath instances for a given path,
 * it uses proper type of classpath depending on a types of particular files containing sources or classes.
 */
class ClassPathFactory {
  /**
    * Create a new classpath based on the abstract file.
    */
  def newClassPath(file: AbstractFile)(using Ctx, CState): ClassPath = ClassPathFactory.newClassPath(file)

  /**
    * Creators for sub classpaths which preserve this context.
    */
  def sourcesInPath(path: String)(using Ctx, CState): List[ClassPath] =
    for {
      file <- expandPath(path, expandStar = false)
      dir <- Option(AbstractFile getDirectory file)
    }
    yield createSourcePath(dir)


  def expandPath(path: String, expandStar: Boolean = true): List[String] = dottyBench.tools.io.ClassPath.expandPath(path, expandStar)

  def expandDir(extdir: String): List[String] = dottyBench.tools.io.ClassPath.expandDir(extdir)

  def contentsOfDirsInPath(path: String)(using Ctx, CState): List[ClassPath] =
    for {
      dir <- expandPath(path, expandStar = false)
      name <- expandDir(dir)
      entry <- Option(AbstractFile.getDirectory(name))
    }
    yield newClassPath(entry)

  def classesInExpandedPath(path: String)(using Ctx, CState): IndexedSeq[ClassPath] =
    classesInPathImpl(path, expand = true).toIndexedSeq

  def classesInPath(path: String)(using Ctx, CState): List[ClassPath] = classesInPathImpl(path, expand = false)

  def classesInManifest(useManifestClassPath: Boolean)(using Ctx, CState): List[ClassPath] =
    if (useManifestClassPath) dottyBench.tools.io.ClassPath.manifests.map(url => newClassPath(AbstractFile getResources url))
    else Nil

  // Internal
  protected def classesInPathImpl(path: String, expand: Boolean)(using Ctx, CState): List[ClassPath] =
    for {
      file <- expandPath(path, expand)
      dir <- {
        def asImage = if (file.endsWith(".jimage")) Some(AbstractFile.getFile(file)) else None
        Option(AbstractFile.getDirectory(file)).orElse(asImage)
      }
    }
    yield newClassPath(dir)

  private def createSourcePath(file: AbstractFile)(using Ctx, CState): ClassPath =
    if (file.isJarOrZip)
      ZipAndJarSourcePathFactory.create(file)
    else if (file.isDirectory)
      new DirectorySourcePath(file.file)
    else
      sys.error(s"Unsupported sourcepath element: $file")
}

object ClassPathFactory {
  def newClassPath(file: AbstractFile)(using Ctx, CState): ClassPath = file match {
    case vd: VirtualDirectory => VirtualDirectoryClassPath(vd)
    case _ =>
      if (file.isJarOrZip)
        ZipAndJarClassPathFactory.create(file)
      else if (file.isDirectory)
        new DirectoryClassPath(file.file)
      else
        sys.error(s"Unsupported classpath element: $file")
  }
}
