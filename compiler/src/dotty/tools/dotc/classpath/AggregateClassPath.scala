/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools
package dotc.classpath

import java.net.URL
import scala.collection.mutable

import dotty.tools.nio.File

/**
 * A classpath unifying multiple class- and sourcepath entries.
 * The Classpath can obtain entries for classes and sources independently
 * so it tries to do operations quite optimally - iterating only these collections
 * which are needed in the given moment and only as far as it's necessary.
 *
 * @param aggregates classpath instances containing entries which this class processes
 */
case class AggregateClassPath(aggregates: Seq[ClassPath]) extends ClassPath {
  // Implementation note:
  // This class is used a lot. It's important to keep it efficient and low-allocation.

  override def findClassFile(className: String): Option[File] = {
    val pkg = PackageNameUtils.separatePackageName(className)
    val iterator = aggregatesForPackage(pkg).iterator
    while iterator.hasNext do
      val file = iterator.next().findClassFile(className)
      if file.nonEmpty then
        return file
    None
  }
  private val packageIndex: collection.mutable.Map[String, Seq[ClassPath]] = collection.mutable.Map()
  private def aggregatesForPackage(pkg: String): Seq[ClassPath] = synchronized {
    packageIndex.getOrElseUpdate(pkg, aggregates.filter(_.hasPackage(pkg)))
  }

  override def asURLs: Iterable[URL] = aggregates.flatMap(_.asURLs)

  override def packages(inPackage: String): Iterable[String] = {
    val result = mutable.HashSet[String]()
    for
      classpath <- aggregates
      pkg <- classpath.packages(inPackage)
    do
      result.add(pkg)
    result
  }

  override def classes(inPackage: String): Iterable[BinaryFileEntry] =
    getDistinctEntries(_.classes(inPackage))

  override def sources(inPackage: String): Iterable[SourceFileEntry] =
    getDistinctEntries(_.sources(inPackage))

  override def hasPackage(pkg: String): Boolean = aggregates.exists(_.hasPackage(pkg))

  private inline def getDistinctEntries[EntryType <: ClassRepresentation](inline getEntries: ClassPath => Iterable[EntryType]): Iterable[EntryType] =
    val seenNames = mutable.HashSet[String]()
    val result = mutable.ArrayBuffer[EntryType]()
    val iterator = aggregates.iterator
    while iterator.hasNext do
      val entries = getEntries(iterator.next()).iterator
      while entries.hasNext do
        val entry = entries.next()
        if seenNames.add(entry.name) then
          result.addOne(entry)
    result
}
