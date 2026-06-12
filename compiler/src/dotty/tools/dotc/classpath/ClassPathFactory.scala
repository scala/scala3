/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.io.{AbstractFile, ClassPath, JarArchive, VirtualDirectory}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.interactive.LogicalSourcePath
import dotty.tools.dotc.interactive.LogicalPackage

import java.nio.file.Files

/**
 * Provides factory methods for classpath. When creating classpath instances for a given path,
 * it uses proper type of classpath depending on a types of particular files containing sources or classes.
 */
class ClassPathFactory(precomputedSourcePackages: Option[LogicalPackage] = None) {

  /**
   * Creators for sub classpaths which preserve this context.
   */
  def sourcesInPath(path: String)(using Context): List[ClassPath] =
    precomputedSourcePackages match {
      // We also accept files in case of YlogicalPackageLoading
      case Some(rootPackage) if ctx.settings.YlogicalPackageLoading.value =>
        List(new LogicalSourcePath(path, rootPackage))
      case _ =>
        for
          file <- expandPath(path, expandStar = false)
          dir <- Option(AbstractFile.getDirectory(file, ctx.settings.javaOutputVersion.value))
        yield ClassPathFactory.newSourcePath(dir)
    }

  def expandPath(path: String, expandStar: Boolean = true): List[String] = dotty.tools.io.ClassPath.expandPath(path, expandStar)

  /** Expand dir out to contents, a la extdir */
  private def expandDir(extdir: String)(using Context): List[String] =
    AbstractFile.getDirectory(extdir, ctx.settings.javaOutputVersion.value) match
      case null => Nil
      case dir => dir.filter(_.isClassContainer).map(x => new java.io.File(dir.file, x.name).getPath).toList

  def contentsOfDirsInPath(path: String)(using Context): List[ClassPath] =
    for {
      dir <- expandPath(path, expandStar = false)
      name <- expandDir(dir)
      entry <- Option(AbstractFile.getDirectory(name, ctx.settings.javaOutputVersion.value))
    }
    yield ClassPathFactory.newClassPath(entry)

  def classesInExpandedPath(path: String)(using Context): IndexedSeq[ClassPath] =
    classesInPathImpl(path, expand = true).toIndexedSeq

  def classesInPath(path: String)(using Context): List[ClassPath] = classesInPathImpl(path, expand = false)

  private def classesInPathImpl(path: String, expand: Boolean)(using Context): List[ClassPath] =
    val files: List[AbstractFile] = for {
      file <- expandPath(path, expand)
      dir <- {
        def asImage = if (file.endsWith(".jimage")) Some(AbstractFile.getFile(file).nn) else None

        Option(AbstractFile.getDirectory(file, ctx.settings.javaOutputVersion.value)).orElse(asImage)
      }
    }
    yield dir

    val expanded =
      if scala.util.Properties.propOrFalse("scala.expandjavacp") then
        for
          file <- files
          a <- JarArchive.expandManifestPath(file.absolutePath)
          path = java.nio.file.Paths.get(a.toURI())
          if Files.exists(path)
        yield
          ClassPathFactory.newClassPath(AbstractFile.getFile(path).nn) // .nn ok because of Files.exists(path)
      else
        Seq.empty

    files.map(ClassPathFactory.newClassPath) ++ expanded

  end classesInPathImpl
}

object ClassPathFactory {
  def newClassPath(file: AbstractFile)(using Context): ClassPath = file match {
    case vd: VirtualDirectory => VirtualDirectoryClassPath(vd)
    case _ =>
      if (file.ext.isJarOrZip)
        ZipAndJarClassPathFactory.create(file)
      else if (file.isDirectory)
        new DirectoryClassPath(file.file.nn)
      else
        sys.error(s"Unsupported classpath element: $file")
  }

  def newSourcePath(file: AbstractFile)(using Context): ClassPath =
    if (file.ext.isJarOrZip)
      ZipAndJarSourcePathFactory.create(file)
    else if (file.isDirectory)
      new DirectorySourcePath(file.file.nn)
    else
      sys.error(s"Unsupported sourcepath element: $file")
}
