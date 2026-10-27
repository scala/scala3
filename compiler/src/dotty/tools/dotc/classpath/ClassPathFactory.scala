/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.io.{AbstractFile, Directory, File, Path, PlainFile, VirtualDirectory}
import dotty.tools.dotc.classpath.FileUtils.isClassContainer
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.interactive.LogicalSourcePath
import dotty.tools.dotc.interactive.LogicalPackage

import java.net.{URI, URISyntaxException}
import java.util.Locale
import java.util.jar.{Attributes, JarInputStream}

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
          file <- ClassPath.expandPath(path, expandStar = false)
          dir <- Option(AbstractFile.getDirectory(file, ctx.settings.javaOutputVersion.value))
        yield ClassPathFactory.newSourcePath(dir)
    }

  /** Expand dir out to contents, a la extdir */
  private def expandDir(extdir: String)(using Context): List[String] =
    AbstractFile.getDirectory(extdir, ctx.settings.javaOutputVersion.value) match
      case null => Nil
      case dir => dir.iterator.filter(_.isClassContainer).map(x => new java.io.File(dir.file, x.name).getPath).toList

  def contentsOfDirsInPath(path: String)(using Context): List[ClassPath] =
    for {
      dir <- ClassPath.expandPath(path, expandStar = false)
      name <- expandDir(dir)
      entry <- Option(AbstractFile.getDirectory(name, ctx.settings.javaOutputVersion.value))
    }
    yield ClassPathFactory.newClassPath(entry)

  def classesInExpandedPath(path: String)(using Context): IndexedSeq[ClassPath] =
    classesInPathImpl(path, expandStar = true).toIndexedSeq

  def classesInPath(path: String)(using Context): List[ClassPath] = classesInPathImpl(path, expandStar = false)

  private def classesInPathImpl(path: String, expandStar: Boolean)(using Context): List[ClassPath] =
    val files: List[AbstractFile] = for {
      file <- ClassPath.expandPath(path, expandStar)
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
          a <- expandManifestPath(file.path)
          if a.exists
        yield
          ClassPathFactory.newClassPath(new PlainFile(a))
      else
        Seq.empty

    files.map(ClassPathFactory.newClassPath) ++ expanded

  end classesInPathImpl


  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  private def expandManifestPath(jarPath: String): List[Path] =
    def resolve(entry: String, baseDir: Directory): Option[Path] =
      // Class-Path may only contain relative URLs, except for the 'file' scheme for compatibility reasons, per
      // https://docs.oracle.com/en/java/javase/17/docs/specs/jar/jar.html#class-path-attribute
      val uri =
        try new URI(entry)
        catch case _: URISyntaxException => return None
      if uri.getScheme == null then
        Some(baseDir.resolve(Path(uri.getPath)))
      else if uri.getScheme.toLowerCase(Locale.ROOT) == "file" then
        Some(Path(java.nio.file.Path.of(uri)))
      else
        None

    val file = File(jarPath)
    if !file.isFile then
      return Nil

    val baseDir = file.parent
    val in = new JarInputStream(file.inputStream())
    val manifest =
      try Option(in.getManifest)
      finally in.close()

    manifest match
      case None => Nil
      case Some(m) =>
        val attrs = m.getMainAttributes.asInstanceOf[java.util.Map[Attributes.Name, String]]
        attrs.get(Attributes.Name.CLASS_PATH) match
          case cp: String if cp.trim().nonEmpty =>
            cp.split("\\s+").toList.flatMap(elem => resolve(elem, baseDir))
          case _ => Nil
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
