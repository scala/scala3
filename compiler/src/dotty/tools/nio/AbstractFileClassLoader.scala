/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools.nio

import java.io.InputStream
import java.net.{URL, URLConnection}
import scala.collection.mutable
import scala.jdk.CollectionConverters.IteratorHasAsJava

class AbstractFileClassLoader(entries: Seq[String | FileContainer], jarVersion: String, parent: ClassLoader) extends ClassLoader(parent):
  def this(dir: String | FileContainer, jarVersion: String, parent: ClassLoader) = this(Seq(dir), jarVersion, parent)

  // Unlike in other places in the compiler, we explicitly support non-existent file containers,
  // since one can create a classloader to load classfiles from, e.g., a directory that doesn't exist yet.
  private val _openedEntries = mutable.ArrayBuffer.empty[FileContainer]
  private val _missingEntries = mutable.ArrayBuffer.empty[String]
  entries.foreach {
    case c: FileContainer => _openedEntries += c
    case s: String => _missingEntries += s
  }

  // Needs to be publicly exposed to be consumed by the eldritch horror that is ClasspathFromClassloader
  def searchLocations: Iterable[String] =
    _openedEntries.map(_.path) ++ _missingEntries

  def add(entry: String): Unit =
    tryOpen(entry) match
      case Some(c) => _openedEntries += c
      case None => _missingEntries += entry

  private def allEntries: Iterable[FileContainer] =
    if _missingEntries.nonEmpty then
      _missingEntries.toList.foreach(e => tryOpen(e) match {
        case Some(c) =>
          _openedEntries += c
          _missingEntries -= e
        case None => ()
      })
    _openedEntries

  // Mimic URLClassLoader's logic of "if it ends in / it's a dir, otherwise it's a JAR"
  private def tryOpen(entry: String): Option[FileContainer] =
    if entry.endsWith("/") then
      FileContainer.getOnDisk(entry)
    else
      File.getOnDisk(entry) match
        case Some(f) => FileContainer.getFromFile(f, jarVersion, FileContainer.DefaultCompressionLevel)
        case _ => None

  override protected def findClass(name: String): Class[?] =
    allEntries.flatMap(_.getFile(name, "class", separator = '.')).headOption.map(file =>
      defineClass(name, file.readBytes())
    ).getOrElse(throw new ClassNotFoundException(name))

  override protected def findResource(name: String): URL | Null =
    val all = findResources(name)
    if all.hasMoreElements then all.nextElement() else null

  // on JDK 20 the URL constructor we're using is deprecated,
  // but the recommended replacement, URL.of, doesn't exist on JDK 17
  @annotation.nowarn("cat=deprecation")
  override protected def findResources(name: String): java.util.Enumeration[URL] =
    allEntries.flatMap(_.getFile(name, separator = '/')).map(file =>
      new URL(null, s"memory:${file.path}", url => new URLConnection(url) {
        override def connect(): Unit = ()
        override def getInputStream: InputStream = file.input()
    })).iterator.asJavaEnumeration

  // overrideable for the REPL
  protected def defineClass(name: String, bytes: Array[Byte]): Class[?] =
    defineClass(name, bytes, 0, bytes.length)
