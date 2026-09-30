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
import java.util.{Collections, Enumeration}

class AbstractFileClassLoader(entries: Seq[AbstractFile], parent: ClassLoader) extends ClassLoader(parent):
  def this(dir: AbstractFile, parent: ClassLoader) = this(Seq(dir), parent)

  private var searchLocations = entries.map(open)

  def add(entry: AbstractFile): Unit =
    searchLocations = open(entry) +: searchLocations

  // used by the REPL
  def root: AbstractFile =
    searchLocations.last

  // Mimic URLClassLoader's logic of "if it ends in / it's a dir, otherwise it's a JAR"
  private def open(entry: AbstractFile): AbstractFile =
    if !entry.exists || entry.isDirectory then entry
    else JarArchive.open(Path(entry.path))

  override def findClass(name: String): Class[?] =
    searchLocations.iterator.flatMap(_.lookupPath(name, '.', lastSuffix = ".class", directory = false)).nextOption().map(file =>
      defineClass(name, file.toByteArray)
    ).getOrElse(throw new ClassNotFoundException(name))

  // on JDK 20 the URL constructor we're using is deprecated,
  // but the recommended replacement, URL.of, doesn't exist on JDK 17
  @annotation.nowarn("cat=deprecation")
  override protected def findResource(name: String): URL | Null =
    searchLocations.iterator.flatMap(_.lookupPath(name, '/', directory = false)).nextOption() match
      case None => null
      case Some(file) => new URL(null, s"memory:${file.path}", url => new URLConnection(url) {
        override def connect(): Unit = ()

        override def getInputStream: InputStream = file.input
      })

  override protected def findResources(name: String): Enumeration[URL] =
    findResource(name) match
      case null => Collections.emptyEnumeration()
      case url => Collections.enumeration(Collections.singleton(url))

  override def loadClass(name: String): Class[?] =
    // Try the parent first. We really don't want to load classes the parent can already load,
    // which can happen with macro stuff like Quotes, because then we have incompatibilities
    // from trying to use a class loaded by the parent with a method that expects a class from us directly.
    try super.loadClass(name)
    catch case _: ClassNotFoundException =>
      val existing = findLoadedClass(name)
      if existing != null then existing
      else findClass(name)

  // overrideable for the REPL
  protected def defineClass(name: String, bytes: Array[Byte]): Class[?] =
    defineClass(name, bytes, 0, bytes.length)
