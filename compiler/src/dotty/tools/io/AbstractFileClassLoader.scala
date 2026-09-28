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

class AbstractFileClassLoader(dirs: Iterable[AbstractFile], parent: ClassLoader) extends ClassLoader(parent):
  def this(dir: AbstractFile, parent: ClassLoader) = this(Seq(dir), parent)

  private var allDirs = dirs

  def addDirectory(dir: AbstractFile): Unit =
    allDirs = Seq(dir) ++ allDirs

  // used by the REPL
  def root: AbstractFile =
    allDirs.last

  private def findClassOption(name: String): Option[Class[?]] =
    dirs.flatMap(_.lookupPath(name, '.', lastSuffix = ".class", directory = false)).headOption.map(file =>
      defineClass(name, file.toByteArray)
    )

  override def findClass(name: String): Class[?] =
    findClassOption(name).getOrElse(throw new ClassNotFoundException(name))

  // on JDK 20 the URL constructor we're using is deprecated,
  // but the recommended replacement, URL.of, doesn't exist on JDK 17
  @annotation.nowarn("cat=deprecation")
  override protected def findResource(name: String): URL | Null =
    dirs.flatMap(_.lookupPath(name, '/', directory = false)).headOption match
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
    val existing = findLoadedClass(name)
    if existing != null then existing
    else findClassOption(name).getOrElse(super.loadClass(name))

  // overrideable for the REPL
  protected def defineClass(name: String, bytes: Array[Byte]): Class[?] =
    defineClass(name, bytes, 0, bytes.length)
