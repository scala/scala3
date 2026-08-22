/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools
package dotc.classpath

import java.net.URL
import dotc.util

import dotty.tools.io.AbstractFile

/**
 * A classpath unifying multiple class- and sourcepath entries.
 * The Classpath can obtain entries for classes and sources independently
 * so it tries to do operations quite optimally - iterating only these collections
 * which are needed in the given moment and only as far as it's necessary.
 *
 * @param aggregates classpath instances containing entries which this class processes
 */
case class AggregateClassPath(aggregates: Seq[ClassPath]) extends ClassPath {
  override def findClassFile(className: String): Option[AbstractFile] = {
    val (pkg, _) = PackageNameUtils.separatePkgAndClassNames(className)
    aggregatesForPackage(pkg).iterator.map(_.findClassFile(className)).collectFirst {
      case Some(x) => x
    }
  }
  private val packageIndex: collection.mutable.Map[String, Seq[ClassPath]] = collection.mutable.Map()
  private def aggregatesForPackage(pkg: String): Seq[ClassPath] = packageIndex.synchronized {
    packageIndex.getOrElseUpdate(pkg, aggregates.filter(_.hasPackage(pkg)))
  }

  override def asURLs: Seq[URL] = aggregates.flatMap(_.asURLs)

  override def packages(inPackage: String): Iterable[String] =
    aggregates.flatMap(_.packages(inPackage)).distinct

  override def classes(inPackage: String): Iterable[BinaryFileEntry] =
    getDistinctEntries(_.classes(inPackage))

  override def sources(inPackage: String): Iterable[SourceFileEntry] =
    getDistinctEntries(_.sources(inPackage))

  override def hasPackage(pkg: String): Boolean = aggregates.exists(_.hasPackage(pkg))

  private def getDistinctEntries[EntryType <: ClassRepresentation](getEntries: ClassPath => Iterable[EntryType]): Iterable[EntryType] =
    val seenFileNames = util.HashSet[String]()
    aggregates.flatMap(getEntries).filter(e => seenFileNames.add(e.fileName))
}
