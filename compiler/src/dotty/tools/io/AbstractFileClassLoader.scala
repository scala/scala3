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

import dotty.tools.nio.*

import java.net.{URL, URLConnection, URLStreamHandler}
import java.util.Collections

class AbstractFileClassLoader(val root: FileContainer, parent: ClassLoader) extends ClassLoader(parent):
  // on JDK 20 the URL constructor we're using is deprecated,
  // but the recommended replacement, URL.of, doesn't exist on JDK 17
  @annotation.nowarn("cat=deprecation")
  override protected def findResource(name: String): URL | Null =
    root.getFile(name, separator = '/') match
      case None => null
      case Some(file) => new URL(null, s"memory:${file.path}", new URLStreamHandler {
        override def openConnection(url: URL): URLConnection = new URLConnection(url) {
          override def connect() = ()
          override def getInputStream = file.input()
        }
      })
  override protected def findResources(name: String): java.util.Enumeration[URL] =
    findResource(name) match
      case null => Collections.enumeration(Collections.emptyList[URL])
      case url  => Collections.enumeration(Collections.singleton(url))

  override def findClass(name: String): Class[?] = {
    root.getFile(name, FileExtension("class"), separator = '.') match
      case None => throw new ClassNotFoundException(name)
      case Some(file) => defineClass(name, file.readBytes())
  }

  // overrideable for the REPL
  protected def defineClass(name: String, bytes: Array[Byte]): Class[?] =
    defineClass(name, bytes, 0, bytes.length)

  override def loadClass(name: String): Class[?] = try findClass(name) catch case _: ClassNotFoundException => super.loadClass(name)
end AbstractFileClassLoader
