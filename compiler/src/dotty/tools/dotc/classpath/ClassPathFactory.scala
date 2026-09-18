/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.nio.*
import dotty.tools.dotc.classpath.FileUtils.isClassContainer
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.interactive.LogicalSourcePath
import dotty.tools.dotc.interactive.LogicalPackage

import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.util.jar.{Attributes, JarInputStream}

/**
 * Provides factory methods for classpath. When creating classpath instances for a given path,
 * it uses proper type of classpath depending on a types of particular files containing sources or classes.
 */
class ClassPathFactory(precomputedSourcePackages: Option[LogicalPackage] = None) {
  private def getContainer(path: String)(using Context): Option[FileContainer] =
    File.getOnDisk(path) match {
      case Some(potentialJar) => FileContainer.getFromFile(potentialJar, ctx.settings.javaOutputVersion.value, ctx.settings.XjarCompressionLevel.value)
      case None => FileContainer.getOnDisk(path)
    }


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
          dir <- getContainer(file)
        yield ClassPathFactory.newSourcePath(dir)
    }

  def expandPath(path: String, expandStar: Boolean = true): List[String] = ClassPath.expandPath(path, expandStar)

  /** Expand dir out to contents, a la extdir */
  private def expandDir(extdir: String)(using Context): Iterable[String] =
    FileContainer.getOnDisk(extdir) match
      case None => Nil
      case Some(dir) => dir.entries.filter(_.isClassContainer).map(_.path)

  def contentsOfDirsInPath(path: String)(using Context): List[ClassPath] =
    for {
      dir <- expandPath(path, expandStar = false)
      name <- expandDir(dir)
      entry <- getContainer(name)
    }
    yield ClassPathFactory.newClassPath(entry)

  def classesInExpandedPath(path: String)(using Context): IndexedSeq[ClassPath] =
    classesInPathImpl(path, expand = true).toIndexedSeq

  def classesInPath(path: String)(using Context): List[ClassPath] = classesInPathImpl(path, expand = false)

  private def classesInPathImpl(path: String, expand: Boolean)(using Context): List[ClassPath] =
    val files: List[File] = 
      for
        file <- expandPath(path, expand)
        dir <- 
          def asImage = Option.when(file.endsWith(".jimage"))(File.getOrCreateOnDisk(file))
          File.getOnDisk(file).orElse(asImage)
      yield dir

    val expanded =
      if scala.util.Properties.propOrFalse("scala.expandjavacp") then
        for
          file <- files
          url <- expandManifestPath(file)
          if url.exists
        yield
          ClassPathFactory.newClassPath(url)
      else
        Seq.empty

    files.map(ClassPathFactory.newClassPath) ++ expanded

  end classesInPathImpl


  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  private def expandManifestPath(jarPath: File): List[URL] =
    def specToURL(spec: String, basedir: FileContainer): Option[URL] =
      try
        val uri = new URI(spec)
        Option.when(uri.isAbsolute)(uri.toURL)
      catch
        case _: MalformedURLException | _: URISyntaxException => None

    val baseDir = jarPath.parent
    val in = new JarInputStream(jarPath.input())
    val manifest =
      try Option(in.getManifest)
      finally in.close()

    manifest match
      case None => Nil
      case Some(m) =>
        val attrs = m.getMainAttributes.asInstanceOf[java.util.Map[Attributes.Name, String]]
        attrs.get(Attributes.Name.CLASS_PATH) match
          case cp: String if cp.trim().nonEmpty =>
            cp.split("\\s+").toList.map(elem => specToURL(elem, baseDir).getOrElse(baseDir.getOrCreateFile(elem).toURL.get))
          case _ => Nil
}
