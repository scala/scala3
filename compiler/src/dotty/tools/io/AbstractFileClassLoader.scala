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

package dotty.tools
package io

import java.io.InputStream
import java.net.{URL, URLConnection}

import scala.jdk.CollectionConverters.IteratorHasAsJava

class AbstractFileClassLoader(entries: Seq[AbstractFile], jarVersion: String, parent: ClassLoader) extends ClassLoader(parent):
  def this(dir: AbstractFile, jarVersion: String, parent: ClassLoader) = this(Seq(dir), jarVersion, parent)

  private var _searchLocations = entries.flatMap(open)

  // Needs to be publicly exposed to be consumed by the eldritch horror that is ClasspathFromClassloader
  def searchLocations: Seq[AbstractFile] =
    _searchLocations

  def add(entry: AbstractFile): Unit =
    _searchLocations = _searchLocations ++ open(entry).toSeq

  // Mimic URLClassLoader's logic of "if it ends in / it's a dir, otherwise it's a JAR"
  private def open(entry: AbstractFile): Option[AbstractFile] =
    if !entry.exists then Some(entry)
    else Option(AbstractFile.getDirectory(entry.path, jarVersion))

  override protected def findClass(name: String): Class[?] =
    searchLocations.iterator.flatMap(_.lookupPath(name, '.', lastSuffix = ".class", directory = false)).nextOption().map(file =>
      defineClass(name, file.toByteArray)
    ).getOrElse(throw new ClassNotFoundException(name))

  override protected def findResource(name: String): URL | Null =
    val all = findResources(name)
    if all.hasMoreElements then all.nextElement() else null

  // on JDK 20 the URL constructor we're using is deprecated,
  // but the recommended replacement, URL.of, doesn't exist on JDK 17
  @annotation.nowarn("cat=deprecation")
  override protected def findResources(name: String): java.util.Enumeration[URL] =
    searchLocations.iterator.flatMap(_.lookupPath(name, '/', directory = false)).map(file =>
      new URL(null, s"memory:${file.path}", url => new URLConnection(url) {
        override def connect(): Unit = ()
        override def getInputStream: InputStream = file.input
    })).asJavaEnumeration

  // overrideable for the REPL
  protected def defineClass(name: String, bytes: Array[Byte]): Class[?] =
    defineClass(name, bytes, 0, bytes.length)
